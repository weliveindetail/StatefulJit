cmake_minimum_required(VERSION 3.0)

project(TestClient)

add_subdirectory(gtest-1.7.0)
enable_testing()

unset(FILES_src)
list(APPEND FILES_src "test.cpp")
#source_group("src" FILES FILES_src)

add_executable(TestClient ${FILES_src})

set_property(TARGET TestClient PROPERTY CXX_STANDARD 14)

target_include_directories(TestClient 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${gtest_SOURCE_DIR} "${gtest_SOURCE_DIR}/include"
)

target_link_libraries(TestClient 
	PRIVATE StatefulJIT gtest
)
