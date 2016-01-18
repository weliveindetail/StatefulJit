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
  bool dumpIR = false;
  auto jitSymbol = CompileTopLevelExpr(jit, dumpIR);

  // get address and reinterpret as function pointer
  double(*FP)() = (double(*)())(intptr_t)jitSymbol.getAddress();

  // run function
  return FP();
}

// ----------------------------------------------------------------------------

TEST(LanguageFeatures, VariableInit)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  // default init
  EXPECT_EQ(0.0, Eval(*jit, "def double a run a;"));
  EXPECT_EQ(0.0, Eval(*jit, "def int b run b;"));

  // init with literal
  EXPECT_EQ(1.0, Eval(*jit, "def double c=1 run c;"));
  EXPECT_EQ(2.0, Eval(*jit, "def int d=2 run d;"));

  // init with literal expression
  EXPECT_EQ(3.0, Eval(*jit, "def double e=1+2 run e;"));
  EXPECT_EQ(4.0, Eval(*jit, "def int f=2*2 run f;"));

  // init with variable
  EXPECT_EQ(5.0, Eval(*jit, "def double g=5, double h=g run h;"));
  EXPECT_EQ(6.0, Eval(*jit, "def int i=6, int j=i run j;"));

  // init with variable expression
  EXPECT_EQ(7.0, Eval(*jit, "def double k=5, double l=k+2 run l;"));
  EXPECT_EQ(8.0, Eval(*jit, "def int m=4, int n=2*m run n;"));
}

// ----------------------------------------------------------------------------

TEST(LanguageFeatures, ImplicitCasts)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  // cast global return value
  EXPECT_EQ(0.0, Eval(*jit, "def int a run a;"));

  // cast init value
  EXPECT_EQ(1.0, Eval(*jit, "def int b=1 run b;"));
  EXPECT_EQ(2.0, Eval(*jit, "def int c=1+1 run c;"));
  EXPECT_EQ(3.0, Eval(*jit, "def double d=3, int e=d run e;"));
  EXPECT_EQ(4.0, Eval(*jit, "def int f=2, double g=f*2 run g;"));

  // cast operations
  EXPECT_EQ(5.0, Eval(*jit, "def int h=2 run h+3;"));
  EXPECT_EQ(6.0, Eval(*jit, "def double i=2, int j=3 run i*j;"));
  EXPECT_EQ(7.0, Eval(*jit, "def int k=3, double l=4 run k+l;"));

  // side effects: rounding
  EXPECT_EQ(8.0, Eval(*jit, "def double m=8.1415, int n=m run n;"));
  EXPECT_EQ(9.0, Eval(*jit, "def double o=4.5, int p=o*2 run p;"));
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
    fflush(stdout);
    getchar();
  #endif

  return exitCode;
}
