# StatefulJit

A minimal experimental Just-in-Time compiler that maintains variable state during recompilation.
This may become interesting especially for 
[live coding systems](https://en.wikipedia.org/wiki/Live_coding).

## Project Setup

StatefulJit is a static library that contains the compiler and the execution engine. It defines 
a very basic API via the StatefulPJ.h used by the two "client" projects InteractiveClient and 
TestClient. The former is useful for playing around and debugging while the latter aims to 
become a comprehensive test suite.

## Language Definition

The language used for this project is a subset of the original 
[kaleidoscope language](http://llvm.org/docs/tutorial/index.html). 
To keep focus and show the basic techniques, control flow and function definition constructs 
have been removed. Example programs are one-liners compiled into a single implicitly defined 
top-level function. The generated code is executed immediately after the compilation finished.

## Example

The following code is part of the TestClient gtest project. It shows how variable states survive 
recompilation. It compiles and executes three programs in sequence on the same JIT and validates 
the return values. The current implemention successfully passes the test case.

```
  auto jit = SetupStatefulJit();
  EXPECT_EQ(1.0, Eval(*jit, "var a=1    in a;"));
  EXPECT_EQ(1.0, Eval(*jit, "var a, b   in a + b;"));
  EXPECT_EQ(3.0, Eval(*jit, "var a, c=2 in a + c;"));
```

`a` is the interesting variable here. All tests declare this variable, but only the first one 
initializes it. In the second and the third program the compiler finds it uninitialized. This 
is the time to start searching for previous revisions of the variable's state. If it's 
successful (as here in the case of `a`) it maps the new variable to the old location in memory. 
Otherwise it allocates new memory on the heap and compiles the default initialization.

## Requirements

Conceptual:
* variables must be allocated on the heap (obviously)
* variable types must have a primitive constructor

Technical:
* 3rd-party libraies: LLVM master installed, gtest binaries
* platforms: Windows 64-bit only (at the moment)
* compilers: Visual Studio 14 2015

## Known Issues

The JIT compiler uses the `llvm/ExecutionEngine/Orc/GlobalMappingLayer.h` class, which has
as little bug in line 35 using the type trait `ModuleSetHandleT`. Within the default 
`ObjectLinkingLayer` this cannot be resolved as it was renamed into `ObjSetHandleT` at 
some point. Renaming it in the header file fixes this problem.
