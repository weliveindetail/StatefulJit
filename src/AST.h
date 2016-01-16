#pragma once

#include <memory>
#include <vector>
#include <string>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

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

// Expression class for referencing a variable, like "a"
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

// Expression class for def/run
class VarSectionExprAST : public ExprAST 
{
public:
  enum class Types
  {
    Undefined,
    Double,
    Int
  };

  class Definition
  {
  public:
    Types type;
    std::string name;
    std::unique_ptr<ExprAST> init = nullptr;
  };

  VarSectionExprAST(
    std::vector<Definition> VarDefs, std::unique_ptr<ExprAST> Body)
    : VarDefinitions(std::move(VarDefs)), Body(std::move(Body)) {}

  llvm::Value* codegen() override;

private:
  std::vector<Definition> VarDefinitions;
  std::unique_ptr<ExprAST> Body;

  llvm::Value* codegenStatefulVarExpr(std::string Name, llvm::Value* InitValue);
  llvm::Value* codegenAllocStatefulVarExpr(std::string Name);
  void codegenRegisterStatefulVarExpr(int VarId, llvm::Value* VoidPtr);
};

// ----------------------------------------------------------------------------

// Expression class for top-level function definition
class TopLevelExprAST
{
  std::unique_ptr<ExprAST> Body;

public:
  TopLevelExprAST(std::unique_ptr<ExprAST> Body) : Body(std::move(Body)) 
  {
  }

  llvm::Function *codegen(
    llvm::orc::StatefulJit& jit,
    llvm::Module* module_rawptr,
    std::string nameId);
};
