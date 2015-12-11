#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>

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
