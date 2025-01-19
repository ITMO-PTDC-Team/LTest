set (SOURCE_TARGET_LIST
    mutexed_register.cpp
    simple_mutex.cpp
    nonlinear_mutex.cpp
    shared_mutexed_register.cpp
    bank.cpp
    buffered_channel.cpp
    simple_deadlock.cpp
    bank_deadlock.cpp
)

set (FOLLY_SOURCE_TARGET_LIST
    folly_sharedmutex.cpp
    folly_rwspinlock.cpp
    folly_flatcombining_queue.cpp
)

foreach(source_name ${SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    add_dependencies(${target} preload)
    list(APPEND VERIFY_BLOCKING_LIST ${target})
endforeach(source_name ${SOURCE_TARGET_LIST})

add_custom_target(verify-blocking 
    DEPENDS
    ${VERIFY_BLOCKING_LIST}
)

function (add_integration_test_blocking test_name label fail)
    set(full_name "${label}_${test_name}")
    add_integration_test(${test_name} ${label} ${fail} ${ARGN})
    set_tests_properties("${full_name}"
        PROPERTIES
        ENVIRONMENT_MODIFICATION "LD_PRELOAD=string_append:${CMAKE_BINARY_DIR}/syscall_intercept/libpreload.so")
endfunction()

add_integration_test_blocking("simple_mutex_pct" "verify" FALSE 
    simple_mutex --strategy pct --rounds 10000
)

add_integration_test_blocking("simple_mutex_random" "verify" FALSE 
    simple_mutex --strategy random --rounds 10000
)

add_integration_test_blocking("nonlinear_mutex_random" "verify" TRUE 
    nonlinear_mutex --strategy random --rounds 10000 --threads 4
)

add_integration_test_blocking("nonlinear_mutex_pct" "verify" TRUE 
    nonlinear_mutex --strategy pct --rounds 10000
)

add_integration_test_blocking("bank_pct" "verify" FALSE 
    bank --strategy pct --rounds 10000
)

add_integration_test_blocking("bank_random" "verify" FALSE 
    bank --strategy random --rounds 10000
)

add_integration_test_blocking("mutexed_register_pct" "verify" FALSE 
    mutexed_register --strategy pct --rounds 10000
)

add_integration_test_blocking("mutexed_register_random" "verify" FALSE 
    mutexed_register --strategy random --rounds 10000
)

add_integration_test_blocking("shared_mutexed_register_pct" "verify" FALSE 
    shared_mutexed_register --strategy pct --rounds 10000
)

add_integration_test_blocking("shared_mutexed_register_random" "verify" FALSE 
    shared_mutexed_register --strategy random --rounds 10000
)

add_integration_test_blocking("buffered_channel_pct" "verify" FALSE 
    buffered_channel --strategy pct --rounds 10000
)

add_integration_test_blocking("buffered_channel_random" "verify" FALSE 
    buffered_channel --strategy random --rounds 10000
)

add_integration_test_blocking("simple_deadlock_pct" "verify" TRUE 
    simple_deadlock --strategy pct --rounds 10000
)

add_integration_test_blocking("simple_deadlock_random" "verify" TRUE 
    simple_deadlock --strategy random --rounds 10000
)

add_integration_test_blocking("bank_deadlock_pct" "verify" TRUE 
    bank_deadlock --strategy pct --rounds 10000
)

add_integration_test_blocking("bank_deadlock_random" "verify" TRUE 
    bank_deadlock --strategy random --rounds 10000
)

foreach(source_name ${FOLLY_SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    target_link_libraries(${target} PRIVATE glog folly double-conversion)
    add_dependencies(${target} preload)
endforeach(source_name ${FOLLY_SOURCE_TARGET_LIST})
