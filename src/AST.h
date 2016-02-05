#pragma once

#include <memory>
#include <vector>
#include <string>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>

namespace llvm {
  namespace orc {
    class StatefulJit;
  }
};

// ----------------------------------------------------------------------------

// Base class for all expression nodes.
class ExprAST 
{
public:
  virtual ~ExprAST() {}
  virtual llvm::Value *codegen() = 0;

  static bool isPrimitiveTypeName(std::string name);
  static llvm::Type* getPrimitiveTypeLlvm(std::string name);
  static llvm::Value* getPrimitiveDefaultInitValue(std::string name);

  static llvm::Value* codegenCastPrimitive(
    llvm::Value* val,
    llvm::Type* dstTy);

private:
  static llvm::Instruction::CastOps getOperationCastPrimitve(
    llvm::Type* srcTy, 
    llvm::Type* dstTy);

  using TypeInfoMap_t = std::map<
    std::string, 
    std::pair<llvm::Type*, llvm::Value*>
  >;

  static TypeInfoMap_t makePrimitiveTypesLlvm();
  static TypeInfoMap_t primitiveTypesLlvm;
};

// ----------------------------------------------------------------------------

// Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST 
{
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  llvm::Value *codegen() override;
};

// ----------------------------------------------------------------------------

// Expression class for referencing a variable
class VariableExprAST : public ExprAST 
{
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  const std::string &getName() const { return Name; }
  llvm::Value *codegen() override;
};

// ----------------------------------------------------------------------------

// Expression class for a binary operator
class BinaryExprAST : public ExprAST 
{
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
    std::unique_ptr<ExprAST> RHS)
    : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  llvm::Value *codegen() override;
};

// ----------------------------------------------------------------------------

// Expression class for defining a variable
class VarDefinitionExprAST : public ExprAST
{
public:
  VarDefinitionExprAST(
    std::string type, std::string name, std::unique_ptr<ExprAST> init)
    : VarTyName(type), VarName(std::move(name)), VarInit(std::move(init)) {}

  llvm::Value* codegen() override;

  llvm::Type* getTy() {
    if (isPrimitiveTypeName(VarTyName)) {
      return getPrimitiveTypeLlvm(VarTyName);
    }
    else {
      assert(false && "Compound types not yet implemented");
      return nullptr;
    }
  }

private:
  std::string VarName;
  std::string VarTyName;
  std::unique_ptr<ExprAST> VarInit = nullptr;

  llvm::Value* codegenStatefulVarExpr(llvm::Value* InitValue);
  llvm::Value* codegenAllocStatefulVarExpr();

  void codegenRegisterStatefulVarExpr(int VarId, llvm::Value* VoidPtr);
};

// ----------------------------------------------------------------------------

// Expression class for top-level function definition
class TopLevelExprAST
{
public:
  void ParseVarSection();
  void ParseBody();

  llvm::Function *codegen(
    llvm::orc::StatefulJit& jit,
    llvm::Module* module_rawptr,
    std::string nameId);

private:
  std::vector<std::unique_ptr<ExprAST>> VarDefinitions;
  std::unique_ptr<ExprAST> Body = nullptr;

};
