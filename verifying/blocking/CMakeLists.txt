set (SOURCE_TARGET_LIST
    mutexed_register.cpp
    simple_mutex.cpp
    nonlinear_mutex.cpp
    shared_mutexed_register.cpp
    bank.cpp
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
endforeach(source_name ${SOURCE_TARGET_LIST})

foreach(source_name ${FOLLY_SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    target_link_libraries(${target} PRIVATE glog folly double-conversion)
    add_dependencies(${target} preload)
endforeach(source_name ${FOLLY_SOURCE_TARGET_LIST})
