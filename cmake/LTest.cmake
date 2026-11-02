include_guard(GLOBAL)

function(ltest_instrument)
    cmake_parse_arguments(PARSE_ARGV 0 ARG "" "" "TARGETS")
    if(ARG_UNPARSED_ARGUMENTS OR NOT ARG_TARGETS)
        message(FATAL_ERROR "Usage: ltest_instrument(TARGETS target...)")
    endif()
    foreach(language C CXX)
        if(CMAKE_${language}_COMPILER_LOADED AND
                (NOT CMAKE_${language}_COMPILER_ID STREQUAL "Clang" OR
                 NOT CMAKE_${language}_COMPILER_VERSION MATCHES "^19\\."))
            message(FATAL_ERROR
                    "LTest instrumentation requires Clang 19; "
                    "configure a separate build with matching C and C++ compilers")
        endif()
    endforeach()
    foreach(target IN LISTS ARG_TARGETS)
        if(NOT TARGET "${target}")
            message(FATAL_ERROR "ltest_instrument: unknown target ${target}")
        endif()
        target_compile_options(${target} PRIVATE
                "$<$<COMPILE_LANGUAGE:C,CXX>:-fpass-plugin=$<TARGET_FILE:LTest::instrumentation>>"
                "$<$<COMPILE_LANGUAGE:C,CXX>:-UNDEBUG>")
        target_link_libraries(${target} PRIVATE LTest::runtime)
        get_target_property(type ${target} TYPE)
        if(type STREQUAL "EXECUTABLE")
            target_link_options(${target} PRIVATE "-Wl,--export-dynamic")
        endif()
    endforeach()
endfunction()

function(ltest_add_test)
    cmake_parse_arguments(PARSE_ARGV 0 ARG "" "NAME;TARGET" "ARGS;LTEST_OPTIONS")
    if(ARG_UNPARSED_ARGUMENTS OR NOT ARG_NAME OR NOT ARG_TARGET)
        message(FATAL_ERROR
                "Usage: ltest_add_test(NAME name TARGET target [ARGS ...] [LTEST_OPTIONS ...])")
    endif()
    get_target_property(type ${ARG_TARGET} TYPE)
    if(NOT type STREQUAL "EXECUTABLE")
        message(FATAL_ERROR "ltest_add_test: ${ARG_TARGET} must be an executable")
    endif()
    add_test(NAME "${ARG_NAME}" COMMAND ${ARG_TARGET}
            ${ARG_LTEST_OPTIONS} -- ${ARG_ARGS})
    set_tests_properties("${ARG_NAME}" PROPERTIES
            LABELS ltest
            ENVIRONMENT_MODIFICATION
                "LD_PRELOAD=string_prepend:$<TARGET_FILE:LTest::preload_with_main>:")
endfunction()
