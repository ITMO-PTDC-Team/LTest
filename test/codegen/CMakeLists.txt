
find_package(Python3 REQUIRED)
#todo how to not hardode paths like this?
find_program(LIT_EXECUTABLE NAMES lit lit.py PATHS /usr/lib/llvm-19/build/utils/lit REQUIRED)
if (NOT LIT_EXECUTABLE)
    message(FATAL_ERROR "Could not find lit testing tool")
endif ()

#todo - in fact it doesn't copy when tests are modified
add_test(NAME build_resmock
        COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/resmock
        ${CMAKE_CURRENT_BINARY_DIR}/resmock
)

add_test(
        NAME "resmock_codegen"
        COMMAND ${LIT_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR} --filter-out
        ${CMAKE_CURRENT_SOURCE_DIR}/resmock/.* --verbose
)

set_tests_properties("resmock_codegen"
        PROPERTIES
        LABELS "llvm-pass"
        DEPENDS build_resmock
)

configure_file(
        "resmock/lit.local.cfg.in"
        "${CMAKE_CURRENT_BINARY_DIR}/resmock/lit.local.cfg"
)

configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.in"
        "${CMAKE_CURRENT_BINARY_DIR}/lit.cfg"
)

