set (SOURCE_TARGET_LIST
    atomic_register.cpp
    deadlock.cpp
    fast_queue.cpp
    mutex_queue.cpp
    race_register.cpp
    nonlinear_queue.cpp
    nonlinear_set.cpp
    nonlinear_ms_queue.cpp
    nonlinear_treiber_stack.cpp
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
endforeach(source_name ${SOURCE_TARGET_LIST})

foreach(source_name ${SOURCE_TARGET_WITHOUT_PLUGIN_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target_without_plugin(${target})
endforeach(source_name ${SOURCE_TARGET_WITHOUT_PLUGIN_LIST})

foreach(source_name ${SOURCE_TARGET_CO_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_cotarget(${target})
endforeach(source_name ${SOURCE_TARGET_CO_LIST})

enable_testing()

add_unit_test("nonlinear_queue_random_minimization" "verify" TRUE 
    nonlinear_queue --tasks 40 --rounds 100000 --strategy random --minimize
)

add_unit_test("nonlinear_queue_pct_minimization" "verify" TRUE 
    nonlinear_queue --tasks 40 --rounds 100000 --strategy pct --minimize
)

add_unit_test("nonlinear_ms_queue_random_minimization" "verify" TRUE 
    nonlinear_ms_queue --tasks 40 --rounds 100000 --strategy random --minimize
)

add_unit_test("nonlinear_ms_queue_pct_minimization" "verify" TRUE 
    nonlinear_ms_queue --tasks 40 --rounds 100000 --strategy pct --minimize
)

add_unit_test("nonlinear_treiber_stack_random_minimization" "verify" TRUE 
    nonlinear_treiber_stack --tasks 40 --rounds 100000 --strategy random --minimize
)

add_unit_test("nonlinear_treiber_stack_pct_minimization" "verify" TRUE 
    nonlinear_treiber_stack --tasks 40 --rounds 100000 --strategy pct --minimize
)

add_unit_test("nonlinear_set_random_minimization" "verify" TRUE 
    nonlinear_set --tasks 40 --rounds 100000 --strategy random --minimize
)

add_unit_test("nonlinear_set_pct_minimization" "verify" TRUE 
    nonlinear_set --tasks 40 --rounds 100000 --strategy pct --minimize
)

add_unit_test("unique_args" "verify" FALSE 
    unique_args
)

add_unit_test("counique_args" "verify" FALSE 
    counique_args
)