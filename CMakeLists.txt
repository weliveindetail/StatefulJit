cmake_minimum_required(VERSION 3.0)

project(StatefulJIT)

### find the correct LLVM installation
if(WIN32)
	set(workaround "ProgramFiles(x86)")
	set(ProgramFiles32Bit "$ENV{${workaround}}")
	set(ProgramFiles64Bit "$ENV{ProgramFiles}")
	set(ProgramFilesWOW64 "$ENV{ProgramW6432}")

	if(CMAKE_SIZEOF_VOID_P EQUAL 8)
		# find LLVM 64bit build at "C:\Program Files\LLVM"
		message(STATUS "Generating for 64bit target")

		if (${ProgramFiles32Bit} STREQUAL ${ProgramFiles64Bit})
			set(ExpectedInstallDir ${ProgramFilesWOW64})
		else()
			set(ExpectedInstallDir ${ProgramFiles64Bit})
		endif()
	else(CMAKE_SIZEOF_VOID_P EQUAL 4)
		# find LLVM 32bit build at "C:\Program Files (x86)\LLVM
		message(STATUS "Generating for 32bit target")
		set(ExpectedInstallDir ${ProgramFiles32Bit})
	else()
		message(FATAL_ERROR "Unable to determine target machine bitwidth.")
	endif()

	set(LLVM_CONFIG_PATH "${ExpectedInstallDir}/LLVM/share/llvm/cmake/LLVMConfig.cmake")
	message(STATUS "Expect LLVM config file at ${LLVM_CONFIG_PATH}")

	include(${LLVM_CONFIG_PATH})
else()
	set(LLVM_REQUIRES_RTTI ON)
	find_package(LLVM REQUIRED)
endif()

### collect the required LLVM link libraries (incl. their dependencies)
llvm_map_components_to_libnames(LLVM_LIBS 
	core 
	orcjit
	x86asmparser
	x86codegen 
)

### allow stdint.h from the C99 standard, as LLVM needs it on Mac
if(XCODE)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS")
endif()

### force static runtime libraries for msvc builds, as gtest needs it on Windows
if(MSVC)
  set(variables 
    CMAKE_CXX_FLAGS_DEBUG
    CMAKE_CXX_FLAGS_RELEASE
  )
  foreach(variable ${variables})
	if(${variable} MATCHES "/MD")
	  string(REGEX REPLACE "/MD" "/MT" ${variable} "${${variable}}")
	endif()
  endforeach()
endif()

message(STATUS "Build flags Debug:" ${CMAKE_CXX_FLAGS_DEBUG})
message(STATUS "Build flags Release: " ${CMAKE_CXX_FLAGS_RELEASE})

### collect source files
unset(FILES_api)
list(APPEND FILES_api "StatefulJitAPI.h")
#source_group("include" FILES FILES_api)

unset(FILES_src)
list(APPEND FILES_src "src/AST.h")
list(APPEND FILES_src "src/CodeGen.h")
list(APPEND FILES_src "src/CodeGen.cpp")
list(APPEND FILES_src "src/Globals.h")
list(APPEND FILES_src "src/Lexer.h")
list(APPEND FILES_src "src/Parser.h")
list(APPEND FILES_src "src/Parser.cpp")
list(APPEND FILES_src "src/StatefulJit.h")
list(APPEND FILES_src "src/StatefulJit.cpp")
#list(APPEND FILES_src "src/VariableRecord.h")
#list(APPEND FILES_src "src/VariableRecord.cpp")
#source_group("src" FILES FILES_src)

### put together the library target
add_library(StatefulJIT ${FILES_api} ${FILES_src})

set_property(TARGET StatefulJIT PROPERTY CXX_STANDARD 14)

target_include_directories(StatefulJIT 
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(StatefulJIT
	PUBLIC ${LLVM_LIBS}
)

### process the clients using the library
add_subdirectory(clients/InteractiveClient)
add_subdirectory(clients/TestClient)
