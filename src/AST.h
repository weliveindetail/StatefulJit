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

  static llvm::Value* codegenCastPrimitive(
    llvm::Value* val,
    llvm::Type* dstTy);

private:
  static llvm::Instruction::CastOps getOperationCastPrimitve(
    llvm::Type* srcTy, 
    llvm::Type* dstTy);
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

class PrimitiveTypeLookup
{
  using TypeInfo_t = std::pair<llvm::Type*, llvm::Value*>;

public:
  PrimitiveTypeLookup();

  bool hasName(std::string name) const;
  llvm::Type* getTypeLlvm(std::string name) const;
  llvm::Value* getDefaultInitValue(std::string name) const;

private:
  std::map<std::string, TypeInfo_t> Map;

};

// ----------------------------------------------------------------------------

class TypeMemberDefinitionExprAST;

// Expression class for defining a type
class TypeDefinitionExprAST : public ExprAST
{
  using MemberDefs_t = std::vector<std::unique_ptr<TypeMemberDefinitionExprAST>>;

public:
  TypeDefinitionExprAST(std::string name, 
                        MemberDefs_t members)
    : TyName(std::move(name))
    , MemberDefs(std::move(members)) {}

  llvm::Value* codegen() override { return nullptr; }
  std::string getTypeName() const { return TyName; }

  llvm::Type* getTy() const {
    if (primitiveTypesLlvm.hasName(TyName)) {
      return primitiveTypesLlvm.getTypeLlvm(TyName);
    }
    else {
      assert(false && "Compound types not yet implemented");
      return nullptr;
    }
  }

  llvm::Value* getDefaultInitVal() const {
    if (primitiveTypesLlvm.hasName(TyName)) {
      return primitiveTypesLlvm.getDefaultInitValue(TyName);
    }
    else {
      assert(false && "Compound types not yet implemented");
      return nullptr;
    }
  }

private:
  std::string TyName;
  MemberDefs_t MemberDefs;

  static const PrimitiveTypeLookup primitiveTypesLlvm;
};

// ----------------------------------------------------------------------------

// Expression class for defining a sub-type
class TypeMemberDefinitionExprAST : public ExprAST
{
public:
  TypeMemberDefinitionExprAST(std::string name, TypeDefinitionExprAST* type)
    : MemberName(std::move(name)), MemberTyDef(type) {}

  llvm::Value* codegen() override { return nullptr; }
  std::string getTypeName() const { return MemberTyDef->getTypeName(); }

private:
  std::string MemberName;
  TypeDefinitionExprAST* MemberTyDef;

};

// ----------------------------------------------------------------------------

// Expression class for defining a variable
class VarDefinitionExprAST : public ExprAST
{
public:
  VarDefinitionExprAST(
    std::string name, TypeDefinitionExprAST* type, std::unique_ptr<ExprAST> init)
    : VarName(std::move(name)), VarTyDef(type), VarInit(std::move(init)) {}

  llvm::Value* codegen() override;

private:
  std::string VarName;
  TypeDefinitionExprAST* VarTyDef;
  std::unique_ptr<ExprAST> VarInit = nullptr;

  llvm::Value* codegenStatefulVarExpr(llvm::Value* InitValue);
  llvm::Value* codegenAlloc();

  void codegenRegisterStatefulVarExpr(int VarId, llvm::Value* VoidPtr);
};

// ----------------------------------------------------------------------------

// Expression class for top-level function definition
class TopLevelExprAST
{
public:
  using TypeDefs_t = std::vector<std::unique_ptr<TypeDefinitionExprAST>>;
  using VarDefs_t = std::vector<std::unique_ptr<ExprAST>>;

  void InitPrimitiveTypes();
  TypeDefinitionExprAST* ResolveTypeDefinition(std::string name);

  void ParseTypeSection();
  void ParseVarSection();
  void ParseBody();

  llvm::Function *codegen(
    llvm::orc::StatefulJit& jit,
    llvm::Module* module_rawptr,
    std::string nameId);

private:
  std::unique_ptr<ExprAST> Body = nullptr;
  TypeDefs_t TypeDefinitions;
  VarDefs_t VarDefinitions;

};
