# StatefulJit

A minimal experimental Just-in-Time compiler that maintains variable state during recompilation.
This may become interesting especially for 
[live coding systems](https://en.wikipedia.org/wiki/Live_coding).

## Table of Contents  
* [Project Structure](#project-structure)
* [Language Definition](#language-definition)
* [Basic Example](#basic-example)
* [Short-Term Goal](#short-term-goal)
* [Build with CMake](#build-with-cmake)

<a name="project-structure"/>
## Project Structure

StatefulJit is a static library that contains the compiler and the execution engine. It defines 
a very basic API via the StatefulJitApi.h used by the two "client" projects InteractiveClient and 
TestClient. The former is useful for playing around and debugging while the latter aims to 
become a comprehensive test suite.

<a name="language-definition"/>
## Language Definition

The language used for this project is a subset of the original 
[kaleidoscope language](http://llvm.org/docs/tutorial/index.html). 
To keep focus and show the basic techniques, control flow and function definition constructs 
have been removed. Example programs are one-liners compiled into a single implicitly defined 
top-level function. The generated code is executed immediately after the compilation finished.

<a name="basic-example"/>
## Basic Example

The following code is part of the TestClient gtest project. It shows how variable states survive 
recompilation. It compiles and executes three programs in sequence on the same JIT and validates 
the return values. The current implemention successfully passes the test case.

```
  auto jit = SetupStatefulJit();
  EXPECT_EQ(1.0, Eval(*jit, "def int a=1           in a;"));
  EXPECT_EQ(1.0, Eval(*jit, "def int a, double b   in a + b;"));
  EXPECT_EQ(3.0, Eval(*jit, "def int a, double c=2 in a + c;"));
```

`a` is the interesting variable here. All tests declare this variable with the same type. 
However, only the first one initializes it. In the second and the third program the compiler 
finds it uninitialized. This is the time to start searching for previous revisions of the 
variable's state. If it's successful (as here in the case of `a`) it maps the new variable 
to the old location in memory. Otherwise it allocates new memory on the heap and compiles 
the default initialization. For more examples have a look at [the gtest](https://github.com/weliveindetail/StatefulJit/blob/master/Clients/TestClient/test.cpp).

<a name="short-term-goal"/>
## Short-Term Goal

Currently the language only covers the primitive types `double` and `int`. As a next step in
implementing my research on the topic, I am going to add compound types and references during 
the next weeks. At this point the project will complete the following test, thus covering the 
basic constructs for type definitions available in typical general-purpose languages.

```
  auto jit = SetupStatefulJit();
  EXPECT_EQ(4.0, Eval(*jit, 
    "type t1: struct { int a, double b };"
    "type t2: struct { t1 a, int b };"
    "type t3: struct { t1& a, t2 b };"
    "def t1 x1 = (1, 2), t2 x2 = (x1, 3), t3 x3 = (x1, x2)"
    "run x2.a.a * x2.a.a + x2.b * x3.b.a.b - x2.b.b"
  ));
```

<a name="build-with-cmake"/>
## Build with CMake

Get CMake ready:
* install the latest CMake for your operating system, you can [find it here](https://cmake.org/)

Get LLVM ready:
* checkout the latest version of LLVM from [SVN trunk](http://llvm.org/svn/llvm-project/llvm/trunk) or the master branch of the [git mirror](https://github.com/llvm-mirror/llvm)
* **on Windows**, find the downloaded sources and **patch the top-level CMakeLists.txt** as [shown here](https://rawgit.com/weliveindetail/StatefulJit/master/docs/patch-llvm-cmakelists.html)
* build and install LLVM with CMake as [described here](http://llvm.org/docs/CMake.html)

Get the sources:
* select a folder of your choice in the command line
* run `git clone https://github.com/weliveindetail/StatefulJit.git`
* create and switch to a build directory using `mkdir StatefulJit/build` and `cd StatefulJit/build`
* generate project files for your preferred development environment with cmake, e.g. `cmake -G "Xcode" ..`

For Windows users:
* get the free non-commercial version of Visual Studio 2015 [from here](https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx) (it's a complete IDE and it will not expire!)
* there is a prepared `configure-msvc.bat` that generates you both, a 32-bit and a 64-bit VS project

For other OS' users:
* you know how it works..
* you'll need a C++14 compatible compiler to build this project
