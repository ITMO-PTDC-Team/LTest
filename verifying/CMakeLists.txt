include_directories(lib)
include_directories(specs)

set (PASS YieldPass)
set (PASS_PATH ${CMAKE_BINARY_DIR}/codegen/lib${PASS}.so)

find_package(Boost REQUIRED COMPONENTS context)

function(verify_target target)
    add_executable(${target} ${source_name})
    add_dependencies(${target} runtime plugin_pass)
    target_include_directories(${target} PRIVATE ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/third_party)
    target_link_options(${target} PRIVATE ${CMAKE_ASAN_FLAGS})
    target_compile_options(${target} PRIVATE -fpass-plugin=${PASS_PATH} ${CMAKE_ASAN_FLAGS})
    target_link_libraries(${target} PRIVATE runtime ${PASS} gflags ${Boost_LIBRARIES})
endfunction()

function(verify_target_without_plugin target)
    add_executable(${target} ${source_name})
    target_include_directories(${target} PRIVATE ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/third_party)
    target_link_options(${target} PRIVATE ${CMAKE_ASAN_FLAGS})
    target_compile_options(${target} PRIVATE ${CMAKE_ASAN_FLAGS})
    target_link_libraries(${target} PRIVATE runtime ${PASS} gflags ${Boost_LIBRARIES})
endfunction()

function(add_unit_test test_name label fail)
    set(full_name "${label}_${test_name}")
    set(test_command ${ARGN})
    add_test(
        NAME "${full_name}"
        COMMAND ${test_command}
    )
    set_tests_properties("${full_name}"
        PROPERTIES
        WILL_FAIL ${fail}
        LABELS "${label}")
endfunction()

add_subdirectory(targets)
add_subdirectory(blocking)
