cmake_minimum_required(VERSION 3.0)

project(InteractiveClient)

unset(FILES_src)
list(APPEND FILES_src "console.cpp")
#source_group("include" FILES FILES_api)

add_executable(InteractiveClient ${FILES_src})

set_property(TARGET InteractiveClient PROPERTY CXX_STANDARD 14)

target_include_directories(InteractiveClient 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
)
	
target_link_libraries(InteractiveClient 
	PRIVATE StatefulJIT
)
