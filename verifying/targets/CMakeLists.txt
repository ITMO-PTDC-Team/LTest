set (SOURCE_TARGET_LIST
    atomic_register.cpp
    fast_queue.cpp
    race_register.cpp
    nonlinear_queue.cpp
    nonlinear_set.cpp
    nonlinear_ms_queue.cpp
    nonlinear_treiber_stack.cpp
    nonlinear_queue_with_custom_round.cpp
    nonlinear_queue_with_multiple_custom_rounds.cpp
    nonlinear_queue_with_large_custom_round.cpp
)

set (SOURCE_TARGET_WITHOUT_PLUGIN_LIST
    unique_args.cpp
)

set (SOURCE_TARGET_CO_LIST
    counique_args.cpp
)

foreach(source_name ${SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    list(APPEND VERIFY_TARGET_LIST ${target})
endforeach(source_name ${SOURCE_TARGET_LIST})

foreach(source_name ${SOURCE_TARGET_WITHOUT_PLUGIN_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target_without_plugin(${target})
    list(APPEND VERIFY_TARGET_LIST ${target})
endforeach(source_name ${SOURCE_TARGET_WITHOUT_PLUGIN_LIST})

foreach(source_name ${SOURCE_TARGET_CO_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_cotarget(${target})
    list(APPEND VERIFY_TARGET_LIST ${target})
endforeach(source_name ${SOURCE_TARGET_CO_LIST})

add_custom_target(verify-targets 
    DEPENDS
    ${VERIFY_TARGET_LIST}
)

add_integration_test("nonlinear_queue_random_minimization" "verify" TRUE 
    nonlinear_queue --tasks 40 --rounds 1000000 --strategy random --minimize
)

add_integration_test("nonlinear_queue_pct_minimization" "verify" TRUE 
    nonlinear_queue --tasks 40 --rounds 1000000 --strategy pct --minimize
)

add_integration_test("nonlinear_ms_queue_random_minimization" "verify" TRUE 
    nonlinear_ms_queue --tasks 40 --rounds 1000000 --strategy random --minimize
)

add_integration_test("nonlinear_ms_queue_pct_minimization" "verify" TRUE 
    nonlinear_ms_queue --tasks 40 --rounds 1000000 --strategy pct --minimize
)

add_integration_test("nonlinear_treiber_stack_random_minimization" "verify" TRUE 
    nonlinear_treiber_stack --tasks 40 --rounds 1000000 --strategy random --minimize
)

add_integration_test("nonlinear_treiber_stack_pct_minimization" "verify" TRUE 
    nonlinear_treiber_stack --tasks 40 --rounds 1000000 --strategy pct --minimize
)

add_integration_test("nonlinear_set_random_minimization" "verify" TRUE 
    nonlinear_set --tasks 40 --rounds 1000000 --strategy random --minimize
)

add_integration_test("nonlinear_set_pct_minimization" "verify" TRUE 
    nonlinear_set --tasks 40 --rounds 1000000 --strategy pct --minimize
)

add_integration_test("nonlinear_queue_pct" "verify" TRUE 
    nonlinear_queue --rounds 10000 --strategy pct
)

add_integration_test("nonlinear_queue_with_custom_round_pct" "verify" TRUE 
    nonlinear_queue_with_custom_round --rounds 0 --exploration_runs 1000 --strategy pct
)

add_integration_test("nonlinear_queue_with_custom_round_random" "verify" TRUE 
    nonlinear_queue_with_custom_round --rounds 0 --exploration_runs 100000 --strategy random
)

add_integration_test("nonlinear_queue_with_large_custom_round_pct" "verify" TRUE 
    nonlinear_queue_with_large_custom_round --rounds 0 --exploration_runs 1000 --strategy pct --minimize
)

add_integration_test("nonlinear_queue_with_large_custom_round_random" "verify" TRUE 
    nonlinear_queue_with_large_custom_round --rounds 0 --exploration_runs 10000 --strategy random --minimize
)

# Runs custom rounds 5 times each, and then generates 1 round with random tasks.
# Note: the number of tasks (--tasks 2) is small so that the nonlinearized scenario is not generated accidentally.
add_integration_test("nonlinear_queue_with_multiple_custom_rounds_pct" "verify" FALSE 
    nonlinear_queue_with_multiple_custom_rounds --rounds 1 --verbose --exploration_runs 5 --strategy pct --tasks 2
)

add_integration_test("nonlinear_queue_with_multiple_custom_rounds_random" "verify" FALSE 
    nonlinear_queue_with_multiple_custom_rounds --rounds 1 --verbose --exploration_runs 5 --strategy random --tasks 2
)

add_integration_test("unique_args" "verify" FALSE 
    unique_args
)

add_integration_test("counique_args" "verify" FALSE 
    counique_args
)