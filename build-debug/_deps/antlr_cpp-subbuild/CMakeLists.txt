# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.25.1)

# We name the project and the target for the ExternalProject_Add() call
# to something that will highlight to the user what we are working on if
# something goes wrong and an error message is produced.

project(antlr_cpp-populate NONE)



include(ExternalProject)
ExternalProject_Add(antlr_cpp-populate
                     "UPDATE_DISCONNECTED" "False" "DOWNLOAD_EXTRACT_TIMESTAMP" "FALSE" "URL" "https://www.antlr.org/download/antlr4-cpp-runtime-4.12.0-source.zip" "URL_HASH" "MD5=acf7371bd7562188712751266d8a7b90"
                    SOURCE_DIR          "/Ltest/build-debug/_deps/antlr_cpp-src"
                    BINARY_DIR          "/Ltest/build-debug/_deps/antlr_cpp-build"
                    CONFIGURE_COMMAND   ""
                    BUILD_COMMAND       ""
                    INSTALL_COMMAND     ""
                    TEST_COMMAND        ""
                    USES_TERMINAL_DOWNLOAD  YES
                    USES_TERMINAL_UPDATE    YES
                    USES_TERMINAL_PATCH     YES
)


