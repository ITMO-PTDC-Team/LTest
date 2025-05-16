cmake_minimum_required(VERSION 3.6)
set (SOURCE_FILES
        lib.cpp
        lin_check.cpp
        logger.cpp
        pretty_printer.cpp
        verifying.cpp 
        generators.cpp
        minimization.cpp
        minimization_smart.cpp
        coro_ctx_guard.cpp
)

add_library(runtime SHARED ${SOURCE_FILES})
find_package(Boost REQUIRED COMPONENTS context)
target_include_directories(runtime PRIVATE include ${Boost_INCLUDE_DIRS})
target_link_libraries(runtime PRIVATE gflags ${Boost_LIBRARIES})
target_link_options(runtime PRIVATE ${CMAKE_ASAN_FLAGS})
target_compile_options(runtime PRIVATE ${CMAKE_ASAN_FLAGS})

