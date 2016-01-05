#include "Codegen.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Verifier.h>

#include "Globals.h"

using namespace llvm;

static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, Value *> NamedValues;

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

Value *VarExprAST::codegen()
{
  auto& C = getGlobalContext();
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // Register all variables and emit their initializer
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
  {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();

    // prevent self-initialization by emitting the 
    // initializer before adding the variable to scope
    Value *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    }
    else { // If not specified, use 0.0.
      InitVal = ConstantFP::get(C, APFloat(0.0));
    }

    BasicBlock* BB = Builder.GetInsertBlock();
    Module* M = BB->getParent()->getParent();

    Type* intTy = Type::getInt64Ty(C);
    Type* bytePtrTy = Type::getInt8PtrTy(C);
    Value* mallocFn = M->getOrInsertFunction("malloc", bytePtrTy, intTy, nullptr);

    Constant* dataSize = ConstantExpr::getSizeOf(Type::getDoubleTy(C));
    CallInst* mallocCall = CallInst::Create(mallocFn, dataSize, VarName + "_void_ptr");
    BB->getInstList().push_back(mallocCall);

    Value* void_ptr = mallocCall;
    Value* double_ptr = Builder.CreateBitCast(void_ptr, Type::getDoublePtrTy(C), VarName + "_double_ptr");
    Builder.CreateStore(InitVal, double_ptr);

    NamedValues[VarName] = double_ptr;
  }

  // Codegen the body and return its computation
  return Body->codegen();
}

// ----------------------------------------------------------------------------

Function *TopLevelExprAST::codegen(Module* module_rawptr, std::string nameId) 
{
  auto& C = getGlobalContext();

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
