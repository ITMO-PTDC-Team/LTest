set (SOURCE_TARGET_LIST
    mutexed_register.cpp
    simple_mutex.cpp
    nonlinear_mutex.cpp
    folly_rwspinlock.cpp
)

foreach(source_name ${SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    add_dependencies(${target} preload)
endforeach(source_name ${SOURCE_TARGET_LIST})

#Worlaround due no folly
list(APPEND VERIFY_BLOCKING_LIST simple_mutex)

add_custom_target(verify-blocking 
    DEPENDS
    ${VERIFY_BLOCKING_LIST}
)

function (add_unit_test_blocking test_name label fail)
    set(full_name "${label}_${test_name}")
    add_unit_test(${test_name} ${label} ${fail} ${ARGN})
    set_tests_properties("${full_name}"
        PROPERTIES
        ENVIRONMENT_MODIFICATION "LD_PRELOAD=string_append:${CMAKE_BINARY_DIR}/syscall_intercept/libpreload.so")
endfunction()

# Does not work, see issue: https://github.com/ITMO-PTDC-Team/LTest/issues/12
# add_unit_test_blocking("simple_mutex_pct" "verify" TRUE 
#     simple_mutex --strategy pct
# )

add_unit_test_blocking("simple_mutex_random" "verify" FALSE 
    simple_mutex --strategy random
)

# Does not work, see issue: https://github.com/ITMO-PTDC-Team/LTest/issues/13
# add_unit_test_blocking("nonlinear_mutex_random" "verify" TRUE 
#     nonlinear_mutex --strategy random
# )

# add_unit_test_blocking("nonlinear_mutex_pct" "verify" TRUE 
#     nonlinear_mutex --strategy pct
# )