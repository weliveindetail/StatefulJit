#include <gtest/gtest.h>

#include <memory>
#include <string>

#include "StatefulJitApi.h"

using llvm::Module;
using llvm::orc::StatefulJit;

// ----------------------------------------------------------------------------

static double Eval(StatefulJit& jit, std::string code)
{
  constexpr auto nameId = "__toplevel_expr";

  // prepare test
  SetupTestInput(code);
  getNextToken();

  // parse and compile top-level function
  auto jitSymbol = CompileTopLevelExpr(jit);

  // get address and reinterpret as function pointer
  double(*FP)() = (double(*)())(intptr_t)jitSymbol.getAddress();

  // run function
  return FP();
}

// ----------------------------------------------------------------------------

TEST(StatefulEvaluation, SingleVariable)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  EXPECT_EQ(0.0, Eval(*jit, "def double a   run a;"));
  EXPECT_EQ(1.0, Eval(*jit, "def double a=1 run a;"));
  EXPECT_EQ(1.0, Eval(*jit, "def double a   run a;"));

  EXPECT_EQ(2.0, Eval(*jit, "def int b=2 run b;"));
  EXPECT_EQ(2.0, Eval(*jit, "def int b   run b;"));
}

// ----------------------------------------------------------------------------

TEST(StatefulEvaluation, MultiVariable)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  EXPECT_EQ(1.0, Eval(*jit, "def double a=1        run a;"));
  EXPECT_EQ(1.0, Eval(*jit, "def double a, int b   run a + b;"));
  EXPECT_EQ(3.0, Eval(*jit, "def double a, int c=2 run a + c;"));
}

// ----------------------------------------------------------------------------

TEST(StatefulEvaluation, RespectPrimitiveTypes)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  EXPECT_EQ(1.0, Eval(*jit, "def double a=1 run a;"));
  EXPECT_EQ(1.0, Eval(*jit, "def double a   run a;")); // type match, reuse definition
  EXPECT_EQ(0.0, Eval(*jit, "def int a      run a;")); // type mismatch, overwrite definition
  EXPECT_EQ(0.0, Eval(*jit, "def double a   run a;")); // type mismatch, overwrite definition
}

// ----------------------------------------------------------------------------

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  int exitCode = RUN_ALL_TESTS();

  // on windows don't close the console window immediately
  #ifdef _WIN32
    getchar();
  #endif

  return exitCode;
}
