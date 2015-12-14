#include <llvm/Analysis/Passes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

#include <cctype>
#include <cstdio>

#include "StatefulPJ.h"

using llvm::Module;
using llvm::orc::StatelessJit;
using llvm::legacy::FunctionPassManager;

// ----------------------------------------------------------------------------

static void HandleTopLevelExpression(StatelessJit& jit)
{
  // collect user input, parse AST and compile top-level function
  auto jitSymbol = CompileTopLevelExpr(jit);

  // get address and reinterpret as function pointer
  double(*FP)() = (double(*)())(intptr_t)jitSymbol.getAddress();

  // evaluate and print out result
  fprintf(stderr, "Evaluated to %f\n", FP());
}

// ----------------------------------------------------------------------------

/// top ::= definition | expression | ';'
static void MainLoop() 
{
  StatelessJit jit(llvm::EngineBuilder().selectTarget());

  fprintf(stderr, "ready> ");
  getNextToken();

  while (1) {
    fprintf(stderr, "ready> ");
    switch (CurTok)
    {
      case tok_eof:
        return;
      case ';': // ignore top-level semicolons.
        getNextToken();
        break;
      default:
        HandleTopLevelExpression(jit);
        break;
    }
  }
}

// ----------------------------------------------------------------------------

int main()
{
  StaticInit();
  MainLoop();
  return 0;
}
