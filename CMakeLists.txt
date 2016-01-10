cmake_minimum_required(VERSION 3.0)

project(StatefulJIT)

find_package(LLVM)

if( NOT LLVM_FOUND )
  message(FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix.")
endif()

set(CMAKE_BUILD_TYPE Debug)
llvm_map_components_to_libnames(LLVM_LIBS 
	core 
	orcjit
	x86asmparser
	x86codegen 
)

# force static runtime libraries for msvc builds
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

unset(FILES_api)
list(APPEND FILES_api "StatefulPJ.h")
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
source_group("src" FILES FILES_src)

# Create a library called "Hello" which includes the source file "hello.cxx".
# The extension is already found. Any number of sources could be listed here.
add_library(StatefulJIT ${FILES_api} ${FILES_src})

target_include_directories(StatefulJIT 
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(StatefulJIT
	PUBLIC ${LLVM_LIBS}
)

add_subdirectory(clients/InteractiveClient)
add_subdirectory(clients/TestClient)

# Make sure the compiler can find include files for our Hello library
# when other libraries or executables link to Hello
#target_include_directories (Hello PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

#target_link_libraries(kscope ast codegen parsing ${REQ_LLVM_LIBRARIES} -lc++)
