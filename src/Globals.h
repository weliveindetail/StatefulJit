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

// helper functions for error handling
inline std::unique_ptr<ExprAST> Error(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

inline std::unique_ptr<ExprAST> ErrorP(const char *Str) {
  Error(Str);
  return nullptr;
}

inline llvm::Value *ErrorV(const char *Str) {
  Error(Str);
  return nullptr;
}
