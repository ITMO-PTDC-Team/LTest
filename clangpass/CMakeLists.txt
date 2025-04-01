#===============================================================================
# SETUP CLANG PLUGIN
#===============================================================================
find_package(Clang REQUIRED CONFIG)
if("${LLVM_VERSION_MAJOR}" VERSION_LESS 19)
  message(FATAL_ERROR "Found LLVM ${LLVM_VERSION_MAJOR}, but need LLVM 19 or above")
endif()

include_directories(SYSTEM "${LLVM_INCLUDE_DIRS};${CLANG_INCLUDE_DIRS}")

if(NOT LLVM_ENABLE_RTTI)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
endif()

# -fvisibility-inlines-hidden is set when building LLVM and on Darwin warnings
# are triggered if llvm-tutor is built without this flag (though otherwise it
# builds fine). For consistency, add it here too.
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-fvisibility-inlines-hidden"
  SUPPORTS_FVISIBILITY_INLINES_HIDDEN_FLAG)
if(${SUPPORTS_FVISIBILITY_INLINES_HIDDEN_FLAG} EQUAL "1")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fvisibility-inlines-hidden")
endif()

add_library(ClangPass SHARED "${CMAKE_CURRENT_SOURCE_DIR}/clangpass.cpp")

target_include_directories(
  ClangPass
  PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/include"
)

# Allow undefined symbols in shared objects on Darwin (this is the default
# behaviour on Linux)
target_link_libraries(ClangPass
  "$<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>")


#===============================================================================
# SETUP CLANG TOOL
#===============================================================================
set(CLANG_TOOL "ClangPassTool")
set(CLANG_TOOL_SOURCES
  "${CMAKE_CURRENT_SOURCE_DIR}/clangpass_tool.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/clangpass.cpp"
)

add_executable(
  ${CLANG_TOOL}
  ${CLANG_TOOL_SOURCES}
)

# Configure include directories for 'tool'
target_include_directories(
  ${CLANG_TOOL}
  PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/include"
)

# Link in the required libraries 
target_link_libraries(
  ${CLANG_TOOL}
  clangTooling
)