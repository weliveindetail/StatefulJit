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
inline static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) 
{
  auto type = Type::getDoubleTy(llvm::getGlobalContext());

  auto& entryBB = TheFunction->getEntryBlock();
  IRBuilder<> TmpB(&entryBB, entryBB.begin());

  return TmpB.CreateAlloca(type, nullptr, VarName.c_str());
}

// ----------------------------------------------------------------------------

class TypeLookup
{
public:
  void init(const llvm::DataLayout& dataLayout);

  bool hasName(std::string name) const;
  llvm::Type* getTypeLlvm(std::string name) const;
  llvm::Value* getDefaultInitValue(std::string name) const;

  void clear();

private:
  llvm::DataLayout* DataLayout_rawptr;
  std::map<std::string, std::pair<llvm::Type*, llvm::Value*>> PrimitiveTypesLlvm;

};
