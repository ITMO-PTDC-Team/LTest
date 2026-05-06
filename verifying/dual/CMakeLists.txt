set(SOURCE_TARGET_LIST
        coro_async_stack.cpp
        coroutine_queue.cpp
        dual_treiber_stack.cpp
)

set(VERIFY_DUAL_LIST)

foreach(source_name ${SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    list(APPEND VERIFY_DUAL_LIST ${target})
endforeach()

function(add_integration_test_dual test_name label fail)
    add_integration_test(${test_name} ${label} ${fail} ${ARGN})
endfunction()

add_integration_test_dual("coro_async_stack_pct" "verify" FALSE
        coro_async_stack --strategy pct --rounds 1000 --deadlock_policy=explore
)

add_custom_target(verify-dual DEPENDS ${VERIFY_DUAL_LIST})
