
find_package(Python3 REQUIRED)
#todo how to not hardode paths like this?
find_program(LIT_EXECUTABLE NAMES lit lit.py PATHS /usr/lib/llvm-19/build/utils/lit REQUIRED)
if (NOT LIT_EXECUTABLE)
    message(FATAL_ERROR "Could not find lit testing tool")
endif ()

add_test(llvm_passes_build
        "${CMAKE_COMMAND}"
        --build "${CMAKE_BINARY_DIR}"
        --config "$<CONFIG>"
        --target ResMockPass YieldPass
)

set_tests_properties(llvm_passes_build PROPERTIES FIXTURES_SETUP codegen_fixture)

set(codegen_tests "resmock" "mainreplacer" "globvariables" )

foreach(item IN LISTS codegen_tests)
    add_test(
            NAME "${item}_codegen"
            COMMAND ${LIT_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR} --filter ${item}/tests/.*  --verbose
    )

    set_tests_properties(${item}_codegen PROPERTIES FIXTURES_REQUIRED codegen_fixture)
endforeach()

configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.in"
        "${CMAKE_CURRENT_BINARY_DIR}/lit.cfg"
)

