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

foreach(source_name ${SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
endforeach(source_name ${SOURCE_TARGET_LIST})

foreach(source_name ${SOURCE_TARGET_WITHOUT_PLUGIN_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target_without_plugin(${target})
endforeach(source_name ${SOURCE_TARGET_WITHOUT_PLUGIN_LIST})
