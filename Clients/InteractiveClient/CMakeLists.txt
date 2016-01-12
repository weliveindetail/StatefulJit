cmake_minimum_required(VERSION 3.0)

project(InteractiveClient)

include_directories( src ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )

unset(FILES_src)
list(APPEND FILES_src "console.cpp")
#source_group("include" FILES FILES_api)

# Create a library called "Hello" which includes the source file "hello.cxx".
# The extension is already found. Any number of sources could be listed here.
add_executable(InteractiveClient ${FILES_src})

target_include_directories(InteractiveClient 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
)
	
target_link_libraries(InteractiveClient 
	PRIVATE StatefulJIT
)

##add_subdirectory(clients/InteractiveClient)
##add_subdirectory(clients/TestClient)

# Make sure the compiler can find include files for our Hello library
# when other libraries or executables link to Hello
#target_include_directories (Hello PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

#target_link_libraries(kscope ast codegen parsing ${REQ_LLVM_LIBRARIES} -lc++)
