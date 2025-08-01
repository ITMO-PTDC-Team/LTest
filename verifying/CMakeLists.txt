include_directories(lib)
include_directories(specs)

set (PASS YieldPass)
set (PASS_PATH ${CMAKE_BINARY_DIR}/codegen/lib${PASS}.so)

set (COPASS CoYieldPass)
set (COPASS_PATH ${CMAKE_BINARY_DIR}/codegen/lib${COPASS}.so)

set(CLANG_TOOL ClangPassTool)
set(CLANG_TOOL_EXECUTABLE ${CMAKE_BINARY_DIR}/clangpass/${CLANG_TOOL})

find_package(Boost REQUIRED COMPONENTS context)

# prefix the actual source file name with '__tmp_' which will be generate by clang pass tool
if (APPLY_CLANG_TOOL)
set(CLANG_TOOL_TMP_PREFIX "__tmp_")
else()
set(CLANG_TOOL_TMP_PREFIX "")
endif()

function(verify_target_without_plugin target)
    if (APPLY_CLANG_TOOL)
        set(SOURCE_FILE_NAME ${CLANG_TOOL_TMP_PREFIX}${source_name})
        add_executable(${target} ${SOURCE_FILE_NAME})
        # apply clangpass to the ${source_name} file
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/${SOURCE_FILE_NAME}
            COMMAND ${CLANG_TOOL_EXECUTABLE}
                    -p=${CMAKE_BINARY_DIR}/compile_commands.json # passing compilation database, make sure CMAKE_EXPORT_COMPILE_COMMANDS flag is set
                    --temp-prefix ${CLANG_TOOL_TMP_PREFIX}
                    --replace-names ::std::mutex,::std::shared_mutex,::std::condition_variable
                    --insert-names  ltest::mutex,ltest::shared_mutex,ltest::condition_variable
                    ${CMAKE_CURRENT_SOURCE_DIR}/${source_name}
            DEPENDS ${CLANG_TOOL}
            COMMENT "Running Clang Pass Tool on ${source_name}"
        )

        # delete the temp file generated by previous command
        add_custom_command(
            TARGET ${target}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E echo "Removing temporary file '${CMAKE_CURRENT_SOURCE_DIR}/${SOURCE_FILE_NAME}' generated after building ${target}"
            COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_CURRENT_SOURCE_DIR}/${SOURCE_FILE_NAME}
        )
    else()
        add_executable(${target} ${source_name})
    endif()
    add_dependencies(${target} runtime plugin_pass)
    target_include_directories(${target} PRIVATE ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/third_party)
    target_link_options(${target} PRIVATE ${CMAKE_ASAN_FLAGS})
    target_compile_options(${target} PRIVATE ${CMAKE_ASAN_FLAGS})
    target_link_libraries(${target} PRIVATE runtime ${PASS} gflags ${Boost_LIBRARIES})
endfunction()

function(verify_target target)
    verify_target_without_plugin(${target})
    add_dependencies(${target} runtime plugin_pass)
    target_compile_options(${target} PRIVATE -fpass-plugin=${PASS_PATH} ${CMAKE_ASAN_FLAGS})
endfunction()

function(verify_cotarget target)
    verify_target_without_plugin(${target})
    add_dependencies(${target} runtime coplugin_pass)
    target_compile_options(${target} PRIVATE  -fplugin=${COPASS_PATH} 
    -fpass-plugin=${COPASS_PATH} -mllvm -coroutine-file=${CMAKE_CURRENT_SOURCE_DIR}/${target}.yml
     ${CMAKE_ASAN_FLAGS})
endfunction()

function(add_integration_test test_name label fail)
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
