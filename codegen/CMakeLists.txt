
find_package(LLVM REQUIRED CONFIG)

include(AddLLVM)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

llvm_add_library(YieldPass 
    SHARED 
    PARTIAL_SOURCES_INTENDED 
    yieldpass.cpp)

llvm_add_library(YieldPassExternal
        SHARED
        PARTIAL_SOURCES_INTENDED
        yieldpass.cpp)
target_compile_definitions(YieldPassExternal PRIVATE LTEST_EXTERNAL_LAUNCH)

llvm_add_library(GlobalVarsPass
        SHARED
        PARTIAL_SOURCES_INTENDED
        globalvariables.cpp)
# https://discourse.cmake.org/t/how-to-add-a-dependency-on-a-compiler-plugin/3077/4
add_custom_target(plugin_pass
    COMMAND ${CMAKE_COMMAND} -E echo "Plugin dependency set"
)
add_dependencies(plugin_pass YieldPass)

add_custom_target(external_plugin_pass
        COMMAND ${CMAKE_COMMAND} -E echo "External plugin dependency set"
)
add_dependencies(external_plugin_pass YieldPassExternal)

add_custom_target(globalvars_plugin_pass
        COMMAND ${CMAKE_COMMAND} -E echo "Global variables plugin dependency set"
)
add_dependencies(globalvars_plugin_pass GlobalVarsPass)

llvm_add_library(CoYieldPass
    SHARED 
    PARTIAL_SOURCES_INTENDED 
    coyieldpass.cpp)

add_custom_target(coplugin_pass
    COMMAND ${CMAKE_COMMAND} -E echo "Plugin dependency set"
)
add_dependencies(coplugin_pass CoYieldPass)


llvm_add_library(ResMockPass
        SHARED
        PARTIAL_SOURCES_INTENDED
        resmockpass.cpp)

add_custom_target(resmock_pass
        COMMAND ${CMAKE_COMMAND} -E echo "Plugin dependency set"
)
add_dependencies(resmock_pass ResMockPass)
