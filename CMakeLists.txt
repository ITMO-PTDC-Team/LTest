cmake_minimum_required(VERSION 3.14)
set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++")
project(lintest)

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(APPLY_CLANG_TOOL ON)

set(CMAKE_CONFIGURATION_TYPES "Debug;Release;RelWithAssert" CACHE STRING "" FORCE)

set(CMAKE_C_FLAGS_RELWITHASSERT "${CMAKE_C_FLAGS_RELEASE} -UNDEBUG" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_RELWITHASSERT "${CMAKE_CXX_FLAGS_RELEASE} -UNDEBUG" CACHE STRING "" FORCE)


set(CMAKE_CXX_FLAGS_DEBUG "-DDEBUG -g -ggdb3 -O0 -fno-omit-frame-pointer")
set(CMAKE_WARN_FLAGS -Wall -Wextra -Werror -pedantic-errors)

if(CMAKE_BUILD_TYPE MATCHES Debug)
    message(STATUS "Debug mode ON")
    set(CMAKE_ASAN_FLAGS -fsanitize=address -fsanitize=undefined -DADDRESS_SANITIZER)
endif(CMAKE_BUILD_TYPE MATCHES Debug)

add_subdirectory(third_party)
include(CTest)
include(GoogleTest)
fuzztest_setup_fuzzing_flags()
enable_testing()

add_subdirectory(clangpass)
add_subdirectory(codegen)
add_subdirectory(runtime)

add_subdirectory(test)
add_subdirectory(verifying)

add_subdirectory(syscall_intercept)
