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
static std::map<std::string, std::pair<TypeDefinition*, Value*>> NamedValues;
static TypeLookup NamedTypes;

// ----------------------------------------------------------------------------

void TypeLookup::clear()
{
  DataLayout_rawptr = nullptr;
  PrimitiveTypesLlvm.clear();
  CompoundTypesLlvm.clear();
}

// ----------------------------------------------------------------------------

void TypeLookup::init(const DataLayout& dataLayout)
{
  DataLayout_rawptr = const_cast<DataLayout*>(&dataLayout);

  Type* doubleTy = Type::getDoubleTy(getGlobalContext());
  Type* intTy = getDefaultIntTy();

  PrimitiveTypesLlvm["double"] = std::make_pair(
    doubleTy, ConstantFP::get(doubleTy, 0.0)
  );

  PrimitiveTypesLlvm["int"] = std::make_pair(
    intTy, ConstantInt::get(intTy, 0, true)
  );
}

// ----------------------------------------------------------------------------

void TypeLookup::populate(const TopLevelExprAST::TypeDefs_t& types)
{
  for (const auto& type : types)
  {
    if (type->IsPrimitive)
    {
      auto it = PrimitiveTypesLlvm.find(type->getTypeName());
      assert(it != PrimitiveTypesLlvm.end() && "Unknwon primitive type");
    }
    else
    {
      bool isNew = !hasName(type->getTypeName());
      assert(isNew && "Ambiguous type name during codegen -- check parser");

      CompoundTypesLlvm[type->getTypeName()] = makeCompound(type);
    }
  }
}

// ----------------------------------------------------------------------------

StructType* TypeLookup::makeCompound(const TypeDef_t& typeDef)
{
  std::vector<Type*> memberTys;

  for (const auto& member : typeDef->MemberDefs)
  {
    Type* ty = getTypeLlvm(member->getTypeName());
    memberTys.push_back(ty);
  }

  auto& ctx = getGlobalContext();
  return StructType::create(ctx, memberTys, typeDef->getTypeName());
}

// ----------------------------------------------------------------------------

bool TypeLookup::hasName(std::string name) const
{
  return PrimitiveTypesLlvm.find(name) != PrimitiveTypesLlvm.end() ||
         CompoundTypesLlvm.find(name) != CompoundTypesLlvm.end();
}

// ----------------------------------------------------------------------------

Type* TypeLookup::getTypeLlvm(std::string name) const
{
  auto itPrim = PrimitiveTypesLlvm.find(name);
  if (itPrim != PrimitiveTypesLlvm.end())
  {
    return itPrim->second.first;
  }

  auto itComp = CompoundTypesLlvm.find(name);
  if (itComp != CompoundTypesLlvm.end())
  {
    return itComp->second;
  }

  assert("Unknown type during codegen -- check parser");
  return nullptr;
}

// ----------------------------------------------------------------------------

Constant* TypeLookup::getDefaultInitValue(std::string name) const
{
  auto itPrim = PrimitiveTypesLlvm.find(name);
  if (itPrim != PrimitiveTypesLlvm.end())
  {
    return itPrim->second.second;
  }

  auto itComp = CompoundTypesLlvm.find(name);
  if (itComp != CompoundTypesLlvm.end())
  {
    return makeDefaultInitValue(itComp->second);
  }

  assert("Unknown type during codegen -- check parser");
  return nullptr;
}

// ----------------------------------------------------------------------------

Constant* TypeLookup::makeDefaultInitValue(Type* ty) const
{
  if (ty->isStructTy())
  {
    std::vector<Constant*> memberDefaults;
    StructType* compoundTy = static_cast<StructType*>(ty);

    for (Type* memberTy : compoundTy->elements())
    {
      Constant* memberDefaultInit = makeDefaultInitValue(memberTy);
      memberDefaults.push_back(memberDefaultInit);
    }

    return ConstantStruct::get(compoundTy, memberDefaults);
  }
  else
  {
    // find primitive default init by llvm type
    // this implementation is not efficient, but there's only a few
    for (const auto& entry : PrimitiveTypesLlvm)
    {
      if (entry.second.first == ty)
        return entry.second.second;
    }
  }

  assert(false);
  return nullptr;
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

Value *VariableExprAST::codegen() 
{
  auto instanceIt = NamedValues.find(Name);
  if (instanceIt == NamedValues.end())
    return ErrorV("Unknown variable name");

  TypeDefinition* def = instanceIt->second.first;
  Value* val = instanceIt->second.second;

  if (MemberAccess.empty())
  {
    return Builder.CreateLoad(val, Name.c_str());
  }
  else
  {
    Type* ty = NamedTypes.getTypeLlvm(def->getTypeName());
    StructType* structTy = static_cast<StructType*>(ty);

    std::vector<Value*> idxList = computeMemberGepIndices(def);
    Value* memberPtr = Builder.CreateInBoundsGEP(structTy, val, idxList);

    return Builder.CreateLoad(memberPtr, Name + "_member");
  }
}

// ----------------------------------------------------------------------------

std::vector<Value*>
VariableExprAST::computeMemberGepIndices(TypeDefinition* def)
{
  Type* idxTy = getDefaultIntTy();
  int memberChainLength = MemberAccess.size();

  std::vector<Value*> idxList;
  idxList.reserve(memberChainLength + 1);

  // initial struct deref, as it is a pointer itself
  idxList.push_back(ConstantInt::get(idxTy, 0, true));

  TypeDefinition* parentTypeDef = def;
  for (int i = 0; i < memberChainLength; i++)
  {
    std::string memberName = MemberAccess[i];
    int memberIdx = parentTypeDef->getMemberIndex(memberName);

    idxList.push_back(ConstantInt::get(idxTy, memberIdx, true));

    parentTypeDef = parentTypeDef->getMemberDef(memberIdx)->getTypeDef();
  }

  return idxList;
}

// ----------------------------------------------------------------------------

int TypeDefinition::getMemberIndex(std::string memberName) const
{
  for (int i = 0; i < MemberDefs.size(); i++)
  {
    if (memberName == MemberDefs[i]->getMemberName())
      return i;
  }

  assert(false && "Unknown member name");
  return -1;
}

// ----------------------------------------------------------------------------

TypeMemberDefinition* TypeDefinition::getMemberDef(int idx) const
{
  return MemberDefs[idx].get();
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

    Value* rawVal = item->second.second;
    Value* typedValue = codegenCastPrimitive(Val, rawVal->getType());

    Builder.CreateStore(typedValue, rawVal);
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

Value* InitExprAST::codegenInit(TypeDefinition* typeDef)
{
  if (PrimitiveInitExpr)
  {
    Value* val = PrimitiveInitExpr->codegen();
    Type* expectedTy = NamedTypes.getTypeLlvm(typeDef->getTypeName());

    if (expectedTy->isStructTy())
    {
      assert(val->getType() == expectedTy);
      return val;
    }
    else
    {
      // primitive types are subject to implicit casting
      return codegenCastPrimitive(val, expectedTy);
    }
  }
  else
  {
    Type* ty = NamedTypes.getTypeLlvm(typeDef->getTypeName());
    StructType* compoundTy = static_cast<StructType*>(ty);

    Value* compoundValPtr = Builder.CreateAlloca(compoundTy);

    for (int i = 0; i < CompoundInitList.size(); i++)
    {
      TypeMemberDefinition* memberDef = typeDef->getMemberDef(i);

      Type* memberTy = NamedTypes.getTypeLlvm(memberDef->getTypeName());
      Value* memberPtr = Builder.CreateStructGEP(compoundTy, compoundValPtr, i);

      Value* initVal = CompoundInitList[i]->codegenInit(memberDef->getTypeDef());

      Builder.CreateStore(initVal, memberPtr);
    }

    return Builder.CreateLoad(compoundValPtr, "tmp");
  }
}

// ----------------------------------------------------------------------------

Value* VarDefinitionExprAST::codegen()
{
  InitExprAST *initExpr_rawptr = VarInit.get();
  Type* ty = NamedTypes.getTypeLlvm(VarTyDef->getTypeName());

  // keep nullptr if there is no explicit init expression
  Value *initVal = nullptr;
  if (initExpr_rawptr)
  {
    initVal = initExpr_rawptr->codegenInit(VarTyDef);
    if (!initVal)
      return nullptr;
  }

  // compile new variable allocation
  Value* valPtr;
  bool requiresInit;

  std::tie(valPtr, requiresInit) = codegenDefinition(ty);

  if (requiresInit && !initVal)
    initVal = NamedTypes.getDefaultInitValue(VarTyDef->getTypeName());

  if (initVal)
    codegenInit(valPtr, ty, initVal);

  NamedValues[VarName] = std::make_pair(VarTyDef, valPtr);
  return valPtr;
}

// ----------------------------------------------------------------------------

void VarDefinitionExprAST::codegenInit(Value* valPtr, Type* valTy, Value* init)
{
  if (valTy->isStructTy())
  {
    assert(init->getType() == valTy);
    Builder.CreateStore(init, valPtr);
  }
  else
  {
    Value* typedInitValue = codegenCastPrimitive(init, valTy);
    Builder.CreateStore(typedInitValue, valPtr);
  }
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
  Type* ptrTy = ty->getPointerTo();
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
  Type* varTy = NamedTypes.getTypeLlvm(VarTyDef->getTypeName());
  Constant* dataSize = ConstantExpr::getSizeOf(varTy);
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
  Type* argVarIdTy = getDefaultIntTy();
  Type* argAddrPtrTy = Type::getInt8PtrTy(C);

  FunctionType* signature =
    FunctionType::get(retTy, { argVarIdTy, argAddrPtrTy }, false);

  Value* submitMemLocFn = M->getOrInsertFunction(
    "SubmitMemoryLocation", signature);

  // compile call
  Constant* varIdConst =
    ConstantInt::get(argVarIdTy, VarId, true);

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

  // setup type lookup
  NamedTypes.init(module_rawptr->getDataLayout());
  NamedTypes.populate(TypeDefinitions);

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
  }
  else
  {
    // error reading body
    topLevelFn->eraseFromParent();
    topLevelFn = nullptr;
  }

  NamedValues.clear();
  NamedTypes.clear();

  return topLevelFn;
}
