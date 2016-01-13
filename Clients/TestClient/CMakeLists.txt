cmake_minimum_required(VERSION 3.0)

project(TestClient)

add_subdirectory(gtest-1.6.0)
enable_testing()

unset(FILES_src)
list(APPEND FILES_src "test.cpp")
#source_group("src" FILES FILES_src)

# Create a library called "Hello" which includes the source file "hello.cxx".
# The extension is already found. Any number of sources could be listed here.
add_executable(TestClient ${FILES_src})

set_property(TARGET TestClient PROPERTY CXX_STANDARD 14)

target_include_directories(TestClient 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${gtest_SOURCE_DIR} "${gtest_SOURCE_DIR}/include"
)

target_link_libraries(TestClient 
	PRIVATE StatefulJIT gtest
)

##add_subdirectory(clients/InteractiveClient)
##add_subdirectory(clients/TestClient)

# Make sure the compiler can find include files for our Hello library
# when other libraries or executables link to Hello
#target_include_directories (Hello PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

#target_link_libraries(kscope ast codegen parsing ${REQ_LLVM_LIBRARIES} -lc++)
