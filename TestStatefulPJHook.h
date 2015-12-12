#include <llvm/Analysis/Passes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

#include "Parser.h"

using llvm::Module;
using llvm::StringRef;
using llvm::orc::KaleidoscopeJIT;
using llvm::legacy::FunctionPassManager;

static int moduleRevision = 0;

static void DeleteJitHistory(KaleidoscopeJIT& jit) 
{
  moduleRevision = 0;
  jit.clearModules();
}

static std::unique_ptr<KaleidoscopeJIT> SetupJit()
{
  moduleRevision = 0;
  return std::make_unique<KaleidoscopeJIT>();
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

static std::unique_ptr<Module> SetupModule(std::string moduleId, const KaleidoscopeJIT& jit)
{
  auto module = std::make_unique<Module>(moduleId, llvm::getGlobalContext());
  module->setDataLayout(jit.getTargetMachine().createDataLayout());

  return module;
}

static std::unique_ptr<FunctionPassManager> SetupPassManager(Module* module_rawptr)
{
  auto fpm = std::make_unique<FunctionPassManager>(module_rawptr);

  fpm->add(llvm::createInstructionCombiningPass()); // simple "peephole" and bit-twiddling optzns.
  fpm->add(llvm::createReassociatePass());          // reassociate expressions
  fpm->add(llvm::createGVNPass());                  // eliminate common sub-expressions
  fpm->add(llvm::createCFGSimplificationPass());    // simplify control flow graph
  fpm->doInitialization();

  return fpm;
}

static double EvaluateTopLevelExpression(KaleidoscopeJIT& jit, std::string code) {
  constexpr auto nameId = "__toplevel_expr";
  auto module_ptr = SetupModule("r" + moduleRevision++, jit);
  auto fpm_ptr = SetupPassManager(module_ptr.get());

  SetupTestInput(code);
  getNextToken();

  // eval top-level expression
  auto ast = ParseTopLevelExpr();
  assert(ast && "Parsing failed");
  
  // generate code into anonymous function
  Function* toplevelFn = ast->codegen(module_ptr.get(), nameId);
  assert(ast && "Code generation failed");

  // JIT compile the owner module
  jit.addModule(std::move(module_ptr));

  // find JIT symbol for compiled function
  auto jitSymbol = jit.findSymbol(nameId);

  // get address and reinterpret as function pointer
  double(*FP)() = (double(*)())(intptr_t)jitSymbol.getAddress();

  // run function
  return FP();
}
