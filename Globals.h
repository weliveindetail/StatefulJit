#pragma once

#include <map>
#include <memory>
#include <string>

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>

#include "AST.h"
#include "KaleidoscopeJIT.h"

using llvm::Module;
using llvm::Function;
using llvm::IRBuilder;
using llvm::AllocaInst;
using llvm::orc::KaleidoscopeJIT;
using llvm::legacy::FunctionPassManager;

static Module* TheModule_rawptr;
static IRBuilder<> Builder(llvm::getGlobalContext());
static std::map<std::string, AllocaInst *> NamedValues;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;

/// Error* - These are little helper functions for error handling.
inline std::unique_ptr<ExprAST> Error(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

inline std::unique_ptr<ExprAST> ErrorP(const char *Str) {
  Error(Str);
  return nullptr;
}

inline Value *ErrorV(const char *Str) {
  Error(Str);
  return nullptr;
}

inline Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule_rawptr->getFunction(Name))
    return F;

  // If no existing prototype exists, return null.
  return nullptr;
}
