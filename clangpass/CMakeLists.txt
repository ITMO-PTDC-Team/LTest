#===============================================================================
# SETUP CLANG PLUGIN
#===============================================================================
find_package(Clang REQUIRED CONFIG)
if("${LLVM_VERSION_MAJOR}" VERSION_LESS 19)
  message(FATAL_ERROR "Found LLVM ${LLVM_VERSION_MAJOR}, but need LLVM 19 or above")
endif()

include_directories(SYSTEM "${LLVM_INCLUDE_DIRS};${CLANG_INCLUDE_DIRS}")
#===============================================================================
# SETUP CLANG TOOL
#===============================================================================
set(CLANG_TOOL "ClangPassTool")
set(CLANG_TOOL_SOURCES
  "${CMAKE_CURRENT_SOURCE_DIR}/clangpass_tool.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/ast_consumer.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/refactor_matcher.cpp"
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
  PRIVATE
  clangTooling
  clangToolingRefactoring
)