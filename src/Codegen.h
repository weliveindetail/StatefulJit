#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/DerivedTypes.h>

#include "AST.h"

using llvm::Type;
using llvm::Function;
using llvm::IRBuilder;
using llvm::AllocaInst;

// Create alloca instruction for mutable variables
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) 
{
  auto type = Type::getDoubleTy(llvm::getGlobalContext());

  auto& entryBB = TheFunction->getEntryBlock();
  IRBuilder<> TmpB(&entryBB, entryBB.begin());

  return TmpB.CreateAlloca(type, nullptr, VarName.c_str());
}

// ----------------------------------------------------------------------------

static Type* getDefaultIntTy()
{
  constexpr int bits = sizeof(int) * 8;
  return Type::getIntNTy(llvm::getGlobalContext(), bits);
}

// ----------------------------------------------------------------------------

class TypeLookup
{
public:
  void init(const llvm::DataLayout& dataLayout);
  void populate(const TopLevelExprAST::TypeDefs_t& types);

  bool hasName(std::string name) const;
  llvm::Type* getTypeLlvm(std::string name) const;
  llvm::Constant* getDefaultInitValue(std::string name) const;

  void clear();

private:
  llvm::DataLayout* DataLayout_rawptr;
  std::map<std::string, llvm::StructType*> CompoundTypesLlvm;
  std::map<std::string, std::pair<llvm::Type*, llvm::Constant*>> PrimitiveTypesLlvm;

  using TypeDef_t = TopLevelExprAST::TypeDefs_t::value_type;
  llvm::StructType* makeCompound(const TypeDef_t& typeDef);

  llvm::Constant* makeDefaultInitValue(llvm::Type* ty) const;
};
