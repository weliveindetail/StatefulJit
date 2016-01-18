#include <llvm/Analysis/Passes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

#include <cctype>
#include <cstdio>

#include "StatefulJitApi.h"

using llvm::Module;
using llvm::orc::StatefulJit;
using llvm::legacy::FunctionPassManager;

// ----------------------------------------------------------------------------

static void HandleTopLevelExpression(StatefulJit& jit)
{
  // collect user input, parse AST and compile top-level function
  bool dumpIR = true;
  auto jitSymbol = CompileTopLevelExpr(jit, dumpIR);

  // get address and reinterpret as function pointer
  double(*FP)() = (double(*)())(intptr_t)jitSymbol.getAddress();

  // evaluate and print out result
  fprintf(stderr, "Evaluated to %f\n", FP());
}

// ----------------------------------------------------------------------------

/// top ::= definition | expression | ';'
static void MainLoop() 
{
  StatefulJit jit(llvm::EngineBuilder().selectTarget());

  fprintf(stderr, "ready> ");
  getNextToken();

  while (1) {
    switch (CurTok)
    {
      case tok_eof:
        return;
      case ';': // ignore top-level semicolons.
        fprintf(stderr, "ready> ");
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
  // failed assertions may not cause abort() on windows
  #ifdef _WIN32
    _CrtSetReportMode(_CRT_ASSERT, _OUT_TO_MSGBOX);
    _set_error_mode(_OUT_TO_MSGBOX);
  #endif

  StaticInit();
  MainLoop();
  return 0;
}
