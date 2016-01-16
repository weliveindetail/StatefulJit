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

// ----------------------------------------------------------------------------

// called from compiled code
extern "C" void SubmitMemoryLocation(int varId, void* ptr)
{
  JitCompiler->submitMemLocation(varId, ptr);
}

// ----------------------------------------------------------------------------

Value *NumberExprAST::codegen() {
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
    // This assume we're building without RTTI because LLVM builds that way by
    // default.  If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
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

    Builder.CreateStore(Val, item->second);
    return Val;
  }

  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return Builder.CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder.CreateFSub(L, R, "subtmp");
  case '*':
    return Builder.CreateFMul(L, R, "multmp");
  case '<':
    L = Builder.CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");

  default:
    return nullptr;
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

  Value* double_ptr = codegenStatefulVarExpr(initVal);
  NamedValues[VarName] = double_ptr;
}

// ----------------------------------------------------------------------------

Value* VarDefinitionExprAST::codegenStatefulVarExpr(Value* InitValue)
{
  auto& C = getGlobalContext();

  // this is minimalistic! add: type, namespace, ..
  int varId = JitCompiler->getOrCreateStatefulVariable(VarName);

  if (JitCompiler->hasMemLocation(varId))
  {
    // variable already existed in previous revision
    void* existingVoidPtr = JitCompiler->getMemLocation(varId);

    // compile address from immediate value
    int ptrBits = sizeof(void*) * 8;
    Type* ptrAsIntTy = Type::getIntNTy(C, ptrBits);
    Constant* addrAsInt = ConstantInt::get(ptrAsIntTy, (int64_t)existingVoidPtr);
    Value* voidPtr = ConstantExpr::getIntToPtr(addrAsInt, Type::getInt8PtrTy(C));

    // cast pointer to double
    Type* ptrTy = Type::getDoublePtrTy(C);
    Value* doublePtr = Builder.CreateBitCast(voidPtr, ptrTy, VarName + "_ptr");

    // overwrite previous value only if init is specified explicitly
    if (InitValue)
    {
      Builder.CreateStore(InitValue, doublePtr);
    }

    return doublePtr;
  }
  else
  {
    // compile new variable allocation
    Value* voidPtr = codegenAllocStatefulVarExpr();

    // submit address to Jit
    codegenRegisterStatefulVarExpr(varId, voidPtr);

    // cast pointer to double
    Type* ptrTy = Type::getDoublePtrTy(C);
    Value* doublePtr = Builder.CreateBitCast(voidPtr, ptrTy, VarName + "_ptr");

    // initialize implicitly if no explicit value provided
    if (!InitValue)
      InitValue = ConstantFP::get(C, APFloat(0.0));

    Builder.CreateStore(InitValue, doublePtr);
    return doublePtr;
  }
}

// ----------------------------------------------------------------------------

Value* VarDefinitionExprAST::codegenAllocStatefulVarExpr()
{
  auto& C = getGlobalContext();
  Module* M = Builder.GetInsertBlock()->getParent()->getParent();

  // declare function
  Type* retTy = Type::getInt8PtrTy(C);
  ArrayRef<Type*> argTys = { Type::getInt64Ty(C) };
  FunctionType* mallocSig = FunctionType::get(retTy, argTys, false);

  Value* mallocFn = M->getOrInsertFunction("malloc", mallocSig);

  // compile call
  Constant* dataSize = ConstantExpr::getSizeOf(Type::getDoubleTy(C));
  CallInst* mallocCall = CallInst::Create(mallocFn, dataSize, VarName + "_void_ptr");
  Builder.GetInsertBlock()->getInstList().push_back(mallocCall);

  return mallocCall; // the symbol the return value of malloc gets stored to
}

// ----------------------------------------------------------------------------

void VarDefinitionExprAST::codegenRegisterStatefulVarExpr(int VarId, Value* VoidPtr)
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

Value *VarSectionExprAST::codegen()
{
  // codegen stateful variables and init
  for (const auto& varDef : VarDefinitions)
  {
    //assert(isa<std::unique_ptr<VarDefinitionExprAST>>(varDef));
    varDef->codegen();
  }

  // codegen the function body and return its computation
  return Body->codegen();
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

  if (Value* retVal = Body->codegen())
  {
    Builder.CreateRet(retVal);
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
