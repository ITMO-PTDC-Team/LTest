
find_package(LLVM REQUIRED CONFIG)

include(AddLLVM)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

llvm_add_library(YieldPass 
    SHARED 
    PARTIAL_SOURCES_INTENDED 
    yieldpass.cpp)

# https://discourse.cmake.org/t/how-to-add-a-dependency-on-a-compiler-plugin/3077/4
add_custom_target(plugin_pass
    COMMAND ${CMAKE_COMMAND} -E echo "Plugin dependency set"
)
add_dependencies(plugin_pass YieldPass)

llvm_add_library(CoYieldPass 
    SHARED 
    PARTIAL_SOURCES_INTENDED 
    coyieldpass.cpp)

add_custom_target(coplugin_pass
    COMMAND ${CMAKE_COMMAND} -E echo "Plugin dependency set"
)
add_dependencies(coplugin_pass CoYieldPass)