#include <gtest/gtest.h>
#include "TestStatefulPJHook.h"

TEST(testMath, myCubeTest)
{
  //EXPECT_EQ(1000, cubic(10));
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  int exitCode = RUN_ALL_TESTS();

  getchar();
  return exitCode;
}
