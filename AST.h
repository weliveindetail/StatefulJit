#pragma once

#include <memory>
#include <vector>
#include <string>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>

using llvm::Value;
using llvm::Function;

// ----------------------------------------------------------------------------

// Base class for all expression nodes.
class ExprAST 
{
public:
  virtual ~ExprAST() {}
  virtual Value *codegen() = 0;
};

// ----------------------------------------------------------------------------

// Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST 
{
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  Value *codegen() override;
};

// ----------------------------------------------------------------------------

// Expression class for referencing a variable, like "a"
class VariableExprAST : public ExprAST 
{
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  const std::string &getName() const { return Name; }
  Value *codegen() override;
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
  Value *codegen() override;
};

// ----------------------------------------------------------------------------

// Expression class for var/in
class VarExprAST : public ExprAST 
{
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;

public:
  VarExprAST(
    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
    std::unique_ptr<ExprAST> Body)
    : VarNames(std::move(VarNames)), Body(std::move(Body)) {}
  Value *codegen() override;
};

// ----------------------------------------------------------------------------

// Expression class for top-level function definition
class TopLevelExprAST 
{
  std::unique_ptr<ExprAST> Body;

public:
  TopLevelExprAST(std::unique_ptr<ExprAST> Body) : Body(std::move(Body)) {}
  Function *codegen();
};
