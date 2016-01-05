#include <gtest/gtest.h>

#include <memory>
#include <string>

#include "StatefulPJ.h"

using llvm::Module;
using llvm::orc::StatelessJit;

// ----------------------------------------------------------------------------

static double Eval(StatelessJit& jit, std::string code)
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
  auto jit = SetupStatelessJit();

  EXPECT_EQ(0.0, Eval(*jit, "var a   in a;"));
  EXPECT_EQ(1.0, Eval(*jit, "var a=1 in a;"));
  EXPECT_EQ(1.0, Eval(*jit, "var a   in a;"));

  EXPECT_EQ(2.0, Eval(*jit, "var b=2 in b;"));
  EXPECT_EQ(2.0, Eval(*jit, "var b   in b;"));
}

// ----------------------------------------------------------------------------

TEST(StatefulEvaluation, MultiVariable)
{
  StaticInit();
  auto jit = SetupStatelessJit();

  EXPECT_EQ(1.0, Eval(*jit, "var a=1    in a;"));
  EXPECT_EQ(1.0, Eval(*jit, "var a, b   in a + b;"));
  EXPECT_EQ(3.0, Eval(*jit, "var a, c=2 in a + c;"));
}

// ----------------------------------------------------------------------------

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  int exitCode = RUN_ALL_TESTS();

  getchar();
  return exitCode;
}
