#!/usr/bin/env bash
# need build directory with  compile_commands.json
clang-tidy  verifying/lib/* verifying/blocking/*.cpp verifying/targets/*.cpp verifying/specs/*.h  \
 test/runtime/*.cpp runtime/include/*.h codegen/*.cpp \
-p build/compile_commands.json  --extra-arg=-std=c++20