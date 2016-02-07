#include "Codegen.h"

#include "Globals.h"
#include "StatefulJit.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Verifier.h>

using namespace llvm;
using llvm::orc::StatefulJit;

static StatefulJit* JitCompiler;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, Value *> NamedValues;
const PrimitiveTypeLookup TypeDefinitionExprAST::primitiveTypesLlvm;

// ----------------------------------------------------------------------------

PrimitiveTypeLookup::PrimitiveTypeLookup()
{
  auto& Ctx = getGlobalContext();
  constexpr int intBits = sizeof(int) * 8;

  // LLVM types
  Map["double"].first = llvm::Type::getDoubleTy(Ctx);
  Map["int"].first = llvm::Type::getIntNTy(Ctx, intBits);

  // default init values
  Map["double"].second = ConstantFP::get(Ctx, APFloat(0.0));
  Map["int"].second = ConstantInt::get(Ctx, APInt(intBits, 0, true));
}

// ----------------------------------------------------------------------------

bool PrimitiveTypeLookup::hasName(std::string name) const
{
  return (Map.find(name) != Map.end());
}

// ----------------------------------------------------------------------------

llvm::Type* PrimitiveTypeLookup::getTypeLlvm(std::string name) const
{
  assert(hasName(name) && "Unknown primitve type name");
  return Map.at(name).first;
}

// ----------------------------------------------------------------------------

Value* PrimitiveTypeLookup::getDefaultInitValue(std::string name) const
{
  assert(hasName(name) && "Unknown primitve type name");
  return Map.at(name).second;
}

// ----------------------------------------------------------------------------

// called from compiled code
extern "C" void SubmitMemoryLocation(int varId, void* ptr)
{
  JitCompiler->submitMemLocation(varId, ptr);
}

// ----------------------------------------------------------------------------

Value *NumberExprAST::codegen() {
  // Number literals are doubles
  return ConstantFP::get(getGlobalContext(), APFloat(Val));
}

// ----------------------------------------------------------------------------

Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (!V)
    return ErrorV("Unknown variable name");

  // Load the value.
  return Builder.CreateLoad(V, Name.c_str());
}

// ----------------------------------------------------------------------------

Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // Assignment requires the LHS to be an identifier.
    VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
    if (!LHSE)
      return ErrorV("destination of '=' must be a variable");

    // Codegen the RHS.
    Value *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    auto item = NamedValues.find(LHSE->getName());
    if (item == NamedValues.end())
      return ErrorV("Unknown variable name");

    Type* varTy = item->second->getType();
    Value* typedValue = codegenCastPrimitive(Val, varTy);

    Builder.CreateStore(typedValue, item->second);
    return Val;
  }

  Value* L = LHS->codegen();
  Value* R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  if (L->getType()->isIntegerTy() && 
      R->getType()->isIntegerTy())
  {
    switch (Op) {
      case '+': return Builder.CreateAdd(L, R, "addtmp");
      case '-': return Builder.CreateSub(L, R, "subtmp");
      case '*': return Builder.CreateMul(L, R, "multmp");
      default: return nullptr;
    }
  }
  else
  {
    auto& C = getGlobalContext();
    Type* retTy = Type::getDoubleTy(C);
    Value* typedL = codegenCastPrimitive(L, retTy);
    Value* typedR = codegenCastPrimitive(R, retTy);

    switch (Op) {
      case '+': return Builder.CreateFAdd(typedL, typedR, "addtmp");
      case '-': return Builder.CreateFSub(typedL, typedR, "subtmp");
      case '*': return Builder.CreateFMul(typedL, typedR, "multmp");
      default: return nullptr;
    }
  }
}

// ----------------------------------------------------------------------------

Value *VarDefinitionExprAST::codegen()
{
  ExprAST *initExpr_rawptr = VarInit.get();

  // keep nullptr if there is no explicit init expression
  Value *initVal = nullptr;
  if (initExpr_rawptr)
  {
    initVal = initExpr_rawptr->codegen();
    if (!initVal)
      return nullptr;
  }

  // compile new variable allocation
  Value* valPtr;
  bool requiresInit;
  Type* ty = VarTyDef->getTy();

  std::tie(valPtr, requiresInit) = codegenDefinition(ty);

  if (requiresInit && !initVal)
    initVal = VarTyDef->getDefaultInitVal();

  if (initVal)
    codegenInit(valPtr, ty, initVal);

  NamedValues[VarName] = valPtr;
}

// ----------------------------------------------------------------------------

void VarDefinitionExprAST::codegenInit(Value* valPtr, Type* valTy, Value* init)
{
  Value* typedInitValue;

  if (valTy->isStructTy())
  {
    // do explicit initialization for compound types

    // implicit initialization has correct type
    assert(valTy == init->getType());
    typedInitValue = init;
  }
  else
  {
    typedInitValue = codegenCastPrimitive(init, valTy);
  }

  Builder.CreateStore(typedInitValue, valPtr);
}

// ----------------------------------------------------------------------------

std::pair<Value*, bool> VarDefinitionExprAST::codegenDefinition(Type* ty)
{
  Value* voidPtr;
  bool requiresInit;

  // this is minimalistic! still only global primitive variables
  int varId = JitCompiler->getOrCreateStatefulVariable(VarName, ty);

  if (JitCompiler->hasMemLocation(varId))
  {
    requiresInit = false;
    voidPtr = codegenReuseMemory(varId);
  }
  else
  {
    requiresInit = true;
    voidPtr = codegenAllocMemory(varId);
    codegenSubmitMemoryLocation(varId, voidPtr);
  }

  // cast pointer to type
  Type* ptrTy = PointerType::getUnqual(ty);
  Value* typedPtr = Builder.CreateBitCast(voidPtr, ptrTy, VarName + "_ptr");

  return std::make_pair(typedPtr, requiresInit);
}

// ----------------------------------------------------------------------------

Value* VarDefinitionExprAST::codegenReuseMemory(int varId)
{
  auto& Ctx = getGlobalContext();

  // variable already existed in previous revision
  void* existingVoidPtr = JitCompiler->getMemLocation(varId);

  // compile address from immediate value
  int ptrBits = sizeof(void*) * 8;
  Type* ptrAsIntTy = Type::getIntNTy(Ctx, ptrBits);
  Constant* addrAsInt = ConstantInt::get(ptrAsIntTy, (int64_t)existingVoidPtr);

  return ConstantExpr::getIntToPtr(addrAsInt, Type::getInt8PtrTy(Ctx));
}

// ----------------------------------------------------------------------------

Value* VarDefinitionExprAST::codegenAllocMemory(int varId)
{
  auto& Ctx = getGlobalContext();
  Module* M = Builder.GetInsertBlock()->getParent()->getParent();

  // declare function
  Type* retTy = Type::getInt8PtrTy(Ctx);
  ArrayRef<Type*> argTys = { Type::getInt64Ty(Ctx) };
  FunctionType* mallocSig = FunctionType::get(retTy, argTys, false);

  Value* mallocFn = M->getOrInsertFunction("malloc", mallocSig);

  // compile call
  Constant* dataSize = ConstantExpr::getSizeOf(VarTyDef->getTy());
  CallInst* mallocCall = CallInst::Create(mallocFn, dataSize, VarName + "_void_ptr");
  Builder.GetInsertBlock()->getInstList().push_back(mallocCall);

  return mallocCall; // the symbol the return value of malloc gets stored to
}

// ----------------------------------------------------------------------------

void VarDefinitionExprAST::codegenSubmitMemoryLocation(int VarId, Value* VoidPtr)
{
  auto& C = getGlobalContext();
  Module* M = Builder.GetInsertBlock()->getParent()->getParent();

  // declare function
  Type* retTy = Type::getVoidTy(C);
  int intBits = sizeof(int) * 8;
  Type* argVarIdTy = Type::getIntNTy(C, intBits);
  Type* argAddrPtrTy = Type::getInt8PtrTy(C);

  FunctionType* signature =
    FunctionType::get(retTy, { argVarIdTy, argAddrPtrTy }, false);

  Value* submitMemLocFn = M->getOrInsertFunction(
    "SubmitMemoryLocation", signature);

  // compile call
  Constant* varIdConst =
    ConstantInt::get(argVarIdTy, APInt(intBits, VarId, true));

  ArrayRef<Value*> params({ varIdConst, VoidPtr });
  CallInst* setMemLocationCall = CallInst::Create(submitMemLocFn, params);
  Builder.GetInsertBlock()->getInstList().push_back(setMemLocationCall);
}

// ----------------------------------------------------------------------------

Value* ExprAST::codegenCastPrimitive(Value* val, Type* dstTy)
{
  if (val->getType() == dstTy)
    return val;

  Instruction::CastOps op = getOperationCastPrimitve(val->getType(), dstTy);

  CastInst* cast = CastInst::Create(op, val, dstTy);
  Builder.GetInsertBlock()->getInstList().push_back(cast);

  return cast;
}

// ----------------------------------------------------------------------------

Instruction::CastOps ExprAST::getOperationCastPrimitve(Type* srcTy, Type* dstTy)
{
  if (srcTy->isDoubleTy() && dstTy->isIntegerTy())
    return Instruction::FPToSI;

  if (srcTy->isIntegerTy() && dstTy->isDoubleTy())
    return Instruction::SIToFP;

  assert(false && "Unknown type conversion");
  return Instruction::BitCast;
}

// ----------------------------------------------------------------------------

Function *TopLevelExprAST::codegen(StatefulJit& jit, 
                                   Module* module_rawptr, 
                                   std::string nameId) 
{
  JitCompiler = &jit;
  auto& C = getGlobalContext();

  // make callback function available in compiled code
  JitCompiler->addGlobalMapping(
    "SubmitMemoryLocation", 
    (void*)SubmitMemoryLocation);

  // declare top-level function
  Type* retTy = Type::getDoubleTy(C);
  FunctionType* signature = FunctionType::get(retTy, false);

  Function *topLevelFn = Function::Create(signature,
    Function::ExternalLinkage, nameId, module_rawptr);

  // prepare codegen
  BasicBlock *BB = BasicBlock::Create(C, "entry", topLevelFn);
  Builder.SetInsertPoint(BB);

  // codegen stateful variables and init
  for (const auto& varDef : VarDefinitions)
  {
    varDef->codegen();
  }

  // codegen Body
  if (Value* retVal = Body->codegen())
  {
    Type* retTy = Type::getDoubleTy(C);
    Value* typedRetVal = ExprAST::codegenCastPrimitive(retVal, retTy);

    Builder.CreateRet(typedRetVal);
    verifyFunction(*topLevelFn);

    topLevelFn->setName(nameId);
    return topLevelFn;
  }
  else
  {
    // error reading body
    topLevelFn->eraseFromParent();
    return nullptr;
  }
}
