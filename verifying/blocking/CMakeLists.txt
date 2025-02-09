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
