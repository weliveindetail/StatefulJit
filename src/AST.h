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

class TypeDefinition;

// Expression class for referencing a variable
class VariableExprAST : public ExprAST 
{
public:
  VariableExprAST(std::string name, 
                  std::vector<std::string> memberAccess)
    : Name(std::move(name))
    , MemberAccess(std::move(memberAccess))
    , CodegenForceReference(false) {}

  std::string getName() const { return Name; }
  llvm::Value *codegen() override;

private:
  std::string Name;
  std::vector<std::string> MemberAccess;
  bool CodegenForceReference;

  llvm::Value* resolveCompoundMemberAccess(
                                    llvm::Value* valuePtr, 
                                    TypeDefinition* typeDef, 
                                    bool isReference);

  llvm::Value* dereferenceCompoundMemberChainItem(
                                    llvm::Value* valPtr,
                                    TypeDefinition* typeDef,
                                    std::vector<llvm::Value*> idxList,
                                    bool dereference);

  // workaround for init expresssions to 
  // force codegen to always return a pointer
  void setCodegenForceReference()
  {
    assert(!CodegenForceReference);
    CodegenForceReference = true;
  }

  friend class InitExprAST;
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

// Codegen class for managing types
class TypeLookup;
class TypeMemberDefinition;

// Expression class for defining a type
class TypeDefinition
{
  using MemberDefs_t = std::vector<std::unique_ptr<TypeMemberDefinition>>;

public:
  TypeDefinition(std::string name, 
                 MemberDefs_t members,
                 bool isPrimitive)
    : TyName(std::move(name))
    , MemberDefs(std::move(members))
    , IsPrimitive(isPrimitive) {}

  std::string getTypeName() const { return TyName; }

  int getMemberIndex(std::string memberName) const;
  TypeMemberDefinition* getMemberDef(int idx) const;

  bool isPrimitve() const { return IsPrimitive; }

private:
  std::string TyName;
  MemberDefs_t MemberDefs;
  bool IsPrimitive;

  friend class TypeLookup;
};

// ----------------------------------------------------------------------------

// Defining a sub-type
class TypeMemberDefinition
{
public:
  TypeMemberDefinition(std::string name, 
                       TypeDefinition* type, 
                       bool isReference)
    : MemberName(std::move(name))
    , MemberTyDef(type) 
    , MemberIsReference(isReference) {}

  std::string getTypeName() const { return MemberTyDef->getTypeName(); }
  std::string getMemberName() const { return MemberName; }
  TypeDefinition* getTypeDef() const { return MemberTyDef; }

  bool isReference() const { return MemberIsReference; }

private:
  std::string MemberName;
  TypeDefinition* MemberTyDef;
  bool MemberIsReference;

};

// ----------------------------------------------------------------------------

// Expression class for initializers and initializer lists
class InitExprAST : public ExprAST
{
public:
  InitExprAST(std::unique_ptr<ExprAST> primitiveInit)
    : PrimitiveInitExpr(std::move(primitiveInit))
    , CompoundInitList() {}

  InitExprAST(std::vector<std::unique_ptr<InitExprAST>> compoundInit)
    : PrimitiveInitExpr(nullptr)
    , CompoundInitList(std::move(compoundInit)) {}

  llvm::Value* codegen() override { return PrimitiveInitExpr->codegen(); };
  llvm::Value* codegenInitExpr(TypeDefinition* typeDef, bool targetIsRefType);

private:
  std::vector<std::unique_ptr<InitExprAST>> CompoundInitList;
  std::unique_ptr<ExprAST> PrimitiveInitExpr;

};

// ----------------------------------------------------------------------------

// Expression class for defining a variable
class VarDefinitionExprAST : public ExprAST
{
public:
  VarDefinitionExprAST(std::string name, 
                       TypeDefinition* type, 
                       std::unique_ptr<InitExprAST> init,
                       bool isReference)
    : VarName(std::move(name))
    , VarTyDef(type)
    , VarInit(std::move(init)) 
    , VarIsReference(isReference) {}

  llvm::Value* codegen() override;

private:
  std::string VarName;
  TypeDefinition* VarTyDef;
  std::unique_ptr<InitExprAST> VarInit;
  bool VarIsReference;

  std::pair<llvm::Value*, bool> 
    codegenDefinition(llvm::Type* ty, llvm::Value* initPtr);

  llvm::Value* codegenReuseMemory(int varId);
  llvm::Value* codegenAllocMemory(int varId);
  void codegenSubmitMemoryLocation(int varId, llvm::Value* voidPtr);
  void codegenInitValue(llvm::Value* valPtr, llvm::Type* valTy, llvm::Value* init);
};

// ----------------------------------------------------------------------------

// Expression class for top-level function definition
class TopLevelExprAST
{
public:
  using TypeDefs_t = std::vector<std::unique_ptr<TypeDefinition>>;
  using VarDefs_t = std::vector<std::unique_ptr<ExprAST>>;

  void InitPrimitiveTypes();
  TypeDefinition* ResolveTypeDefinition(std::string name);

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
