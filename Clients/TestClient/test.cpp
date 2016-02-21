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

TEST(LanguageFeatures, Minimal)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  EXPECT_EQ(0.0, Eval(*jit, "run 0;"));
  EXPECT_EQ(1.0, Eval(*jit, "run 1;"));
  EXPECT_EQ(2.0, Eval(*jit, "run 3-2+1;"));
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

TEST(LanguageFeatures, FlatCompoundTypeDefinitions)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  EXPECT_EQ(0.0, Eval(*jit, "types t1: struct { int a } run 0;"));
  EXPECT_EQ(1.0, Eval(*jit, "types t1: struct { int a, double b } run 1;"));
  EXPECT_EQ(2.0, Eval(*jit, "types t1: struct { int a }, "
                                  "t2: struct { int a, double b }, "
                                  "t3: struct { double a } run 2;"));
}

// ----------------------------------------------------------------------------

TEST(LanguageFeatures, FlatCompoundTypeInstantiation)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  // default initialization
  EXPECT_EQ(0.0, Eval(*jit, 
    "types t1: struct { int a }" 
    "def t1 x1 run 0;"
  ));
  EXPECT_EQ(0.0, Eval(*jit, 
    "types t2: struct { int b }"
    "def t2 x2 run x2.b;"
  ));
  EXPECT_EQ(0.0, Eval(*jit, 
    "types t3: struct { int c, double d }"
    "def t3 x3 run x3.c + x3.d;"
  ));

  // explicit initialization
  EXPECT_EQ(1.0, Eval(*jit, 
    "types t4: struct { int e }"
    "def t4 x4 = (1) run x4.e;"
  ));
  EXPECT_EQ(2.0, Eval(*jit, 
    "types t5: struct { int f, double g }"
    "def t5 x5 = (2 + 1, 1) run x5.f - x5.g;"
  ));
  EXPECT_EQ(3.0, Eval(*jit, R"(
    types t6: struct { int h, double i }
    def double j = 3,
        t6 x6 = (j, j * j)
    run x6.i - 2 * x6.h;
  )"));
  EXPECT_EQ(4.0, Eval(*jit, R"(
    types t7: struct { int k },
          t8: struct { double l, int m }
    def t7 x7 = (1 - 2),
        t8 x8 = (x7.k, x7.k * x7.k)
    run 3 * x8.m - x7.k;
  )"));
  EXPECT_EQ(5.0, Eval(*jit, R"(
    types t9: struct { int n }
    def t9 x9 = (5), t9 y9 = x9 run y9.n;
  )"));
}

// ----------------------------------------------------------------------------

TEST(LanguageFeatures, NestedCompoundTypeDefinitions)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  EXPECT_EQ(0.0, Eval(*jit,
    "types t1: struct { int a }, "
    "      t2: struct { t1 a } run 0;"
  ));

  EXPECT_EQ(0.0, Eval(*jit,
    "types t3: struct { double a }, "
    "      t4: struct { double a, t3 b },"
    "      t5: struct { double a, t3 b, t4 c } run 0;"
  ));
}

// ----------------------------------------------------------------------------

TEST(LanguageFeatures, NestedCompoundTypeInstantiation)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  // default initialization
  EXPECT_EQ(0.0, Eval(*jit, R"(
    types t1: struct { int a },
          t2: struct { t1 a }
    def t1 x1, t2 x2 run x1.a - x2.a.a;
  )"));

  EXPECT_EQ(0.0, Eval(*jit, R"(
    types t3: struct { double a },
          t4: struct { double a, t3 b },
          t5: struct { double a, t3 b, t4 c }
    def t3 x3, t4 x4, t5 x5 
    run x3.a - x4.a + x4.b.a - x5.b.a + x5.c.b.a;
  )"));

  // explicit initialization
  EXPECT_EQ(1.0, Eval(*jit, R"(
    types t6: struct { int a },
          t7: struct { t6 b }
    def t6 x6 = (1), t7 x7 = (x6) run x7.b.a;
  )"));

  EXPECT_EQ(2.0, Eval(*jit, R"(
    types t8: struct { int a },
          t9: struct { int a, t8 b }
    def t9 x9 = (1, (2)) run x9.b.a;
  )"));

  EXPECT_EQ(3.0, Eval(*jit, R"(
    types t10: struct { int a },
          t11: struct { t10 b }
    def t11 x11 = ((3)), t11 y11 = x11 run y11.b.a;
  )"));

  EXPECT_EQ(4.0, Eval(*jit, R"(
    types t12: struct { double a },
          t13: struct { double a, t12 b },
          t14: struct { double a, t12 b, t13 c }
    def t12 x12 = (1), t13 x13 = (1, x12), t14 x14 = (4, x12, x13)
    run x12.a - x13.b.a + x13.b.a - x14.c.b.a + x14.a;
  )"));

  EXPECT_EQ(5.0, Eval(*jit, R"(
    types t15: struct { double a },
          t16: struct { double a, t15 b },
          t17: struct { double a, t15 b, t16 c },
          t18: struct { t17 d }
    def t18 x18 = ((1, (2), (3, (4)))),
        t17 x17 = x18.d, 
        t16 x16 = x17.c, 
        t15 x15 = x17.b, 
        t17 y17 = (x17.a, x17.b, x17.c),
        t16 y16 = (x17.c.a, x17.c.b),
        t15 y15 = (x17.b.a)
    run x17.a - y17.a + x17.b.a - y17.b.a + x17.c.b.a - y17.c.b.a +
        x16.a - y16.a + x16.b.a - y16.b.a +
        x15.a - y15.a +
        5;
  )"));
}

// ----------------------------------------------------------------------------

TEST(LanguageFeatures, References)
{
  StaticInit();
  auto jit = SetupStatefulJit();

  // primitive type reference variables
  EXPECT_EQ(0.0, Eval(*jit, "def double a, double& b=a run b;"));
  EXPECT_EQ(1.0, Eval(*jit, "def double c=1, double& d=c run d;"));
  EXPECT_EQ(2.0, Eval(*jit, "def int e=1+1, int& f=e run f;"));
  EXPECT_EQ(3.0, Eval(*jit, "def int g=3, int& h=g, int i=h run i;"));
  EXPECT_EQ(4.0, Eval(*jit, "def int j=2, int& k=j, int& l=k run k+l;"));

  // compound type reference variables
  EXPECT_EQ(5.0, Eval(*jit, R"(
    types t1: struct { int a, double b }
    def t1 x1 = (2, 3), t1& y1 = x1 run y1.a + y1.b;
  )"));

  // primitive type reference members
  EXPECT_EQ(6.0, Eval(*jit, R"(
    types t2: struct { int a, double& b }
    def double p = 2, t2 x2 = (4, p), t2& y2 = x2 run y2.a + y2.b;
  )"));

  // compound type reference members
  EXPECT_EQ(7.0, Eval(*jit, R"(
    types t3: struct { int a },
          t4: struct { t3& b }
    def t3 x3 = (7), t4 x4 = (x3) run x4.b.a;
  )"));
  EXPECT_EQ(8.0, Eval(*jit, R"(
    types t5: struct { int a },
          t6: struct { t5& b }
    def t5 x5 = (8), t5& y5 = x5, t6 x6 = (y5) run x6.b.a;
  )"));
  EXPECT_EQ(9.0, Eval(*jit, R"(
    types t7: struct { int a },
          t8: struct { t7& b }
    def t7 x7 = (9), t8 x8 = (x7), t7 y7 = x8.b run y7.a;
  )"));
  EXPECT_EQ(12.0, Eval(*jit, R"(
    types t11: struct { int& a },
          t12: struct { t11& b }
    def int q = 12, t11 x11 = (q), t11& y11 = x11, t12 x12 = (y11) run x12.b.a;
  )"));
  /*EXPECT_EQ(10.0, Eval(*jit, R"(
    types 
      t10: struct { int a, double b, int& c, double& d },
      t11: struct { int a, double b, t10& x, double& d },
      t12: struct { int a, t11&   x, int& c, double& d },
      t13: struct { int a, double b, int& c, t12     x },
      t14: struct { t13 x, double b, int& c, double& d },
      t15: struct { t14 x, double b, int& c, double& d },
      t16: struct { int a, double b, int& c, t15&    x },
      t17: struct { int a, t16    x, int& c, double& d },
      t18: struct { int a, double b, t17& x, double& d }

    def 
      int i = 1, 
      double f = 2,
      t10 x10 = (f, i, i, f), 
      t11 x11 = ( y7 = x7, t8 x8 = (y7) run x8.b.a;
  )"));*/
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
  ::testing::GTEST_FLAG(catch_exceptions) = false;

  int exitCode = RUN_ALL_TESTS();

  // on windows don't close the console window immediately
  #ifdef _WIN32
    fflush(stdout);
    getchar();
  #endif

  return exitCode;
}
