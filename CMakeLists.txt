cmake_minimum_required(VERSION 3.14)
set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++")
project(lintest)

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# TODO(kmitkin): require to understand, what is it considered to be "optimized" build
# set(CMAKE_CXX_FLAGS_RELEASE "???")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

if(CMAKE_BUILD_TYPE MATCHES Debug)
    message(STATUS "Debug mode ON")
    set(CMAKE_CXX_FLAGS_DEBUG "-DDEBUG -g -ggdb3 -O0 -fno-omit-frame-pointer")
    set(CMAKE_WARN_FLAGS -Wall -Wextra -Werror -pedantic-errors)
    set(CMAKE_ASAN_FLAGS -fsanitize=address -fsanitize=undefined -DADDRESS_SANITIZER)
endif(CMAKE_BUILD_TYPE MATCHES Debug)

add_subdirectory(third_party)

include(GoogleTest)
fuzztest_setup_fuzzing_flags()
enable_testing()

add_subdirectory(codegen)
add_subdirectory(runtime)

add_subdirectory(test)
add_subdirectory(verifying)

add_subdirectory(syscall_intercept)
