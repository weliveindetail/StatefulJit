#pragma once

#include <llvm/Analysis/Passes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

#include "src/Parser.h"
#include "src/Codegen.h"
#include "src/StatefulJit.h"

using llvm::Module;
using llvm::orc::JITSymbol;
using llvm::orc::StatefulJit;
using llvm::legacy::FunctionPassManager;

static int moduleRevision = 0;

static void DeleteJitHistory(StatefulJit& jit)
{
  moduleRevision = 0;
  jit.clearModules();
}

static std::unique_ptr<StatefulJit> SetupStatefulJit()
{
  moduleRevision = 0;
  auto* targetMachine_rawptr = llvm::EngineBuilder().selectTarget();
  return std::make_unique<StatefulJit>(targetMachine_rawptr);
}

static void StaticInit()
{
  // LLVM target selection
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // supported binary operators
  BinopPrecedence['='] = 2;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest
}

static std::unique_ptr<Module> SetupModule(std::string moduleId, const StatefulJit& jit)
{
  auto module = std::make_unique<Module>(moduleId, llvm::getGlobalContext());
  module->setDataLayout(jit.getTargetMachine().createDataLayout());

  return module;
}

static JITSymbol CompileTopLevelExpr(StatefulJit& jit)
{
  constexpr auto nameId = "__toplevel_expr";

  auto module_ptr = SetupModule("r" + moduleRevision++, jit);

  // eval top-level expression
  auto topLevelAst = ParseTopLevelExpr();
  assert(topLevelAst && "Parsing failed");

  // generate code into anonymous function
  Function* toplevelFn = topLevelAst->codegen(jit, module_ptr.get(), nameId);
  assert(toplevelFn && "Code generation failed");

  toplevelFn->dump();

  // JIT compile the owner module
  jit.addModule(std::move(module_ptr));

  // find JIT symbol for compiled function
  return jit.findSymbol(nameId);
}
