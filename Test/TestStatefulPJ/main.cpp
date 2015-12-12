#include <gtest/gtest.h>
#include "TestStatefulPJHook.h"

TEST(testStatefulPJ, Basics)
{
  StaticInit();
  std::unique_ptr<KaleidoscopeJIT> jit = SetupJit();

  EXPECT_EQ(0.0, Eval(*jit, "var a in a;"));
  EXPECT_EQ(1.0, Eval(*jit, "var a=1 in a;"));
  EXPECT_EQ(2.0, Eval(*jit, "var a=1, b=2 in b;"));
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  int exitCode = RUN_ALL_TESTS();

  getchar();
  return exitCode;
}
