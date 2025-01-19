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

add_executable(folly_sharedmutex folly_sharedmutex.cpp)
target_include_directories(folly_sharedmutex PRIVATE ${CMAKE_SOURCE_DIR})
target_link_options(folly_sharedmutex PRIVATE ${CMAKE_ASAN_FLAGS})
add_dependencies(folly_sharedmutex preload)
target_link_libraries(folly_sharedmutex PRIVATE runtime gflags glog)
target_link_libraries(folly_sharedmutex PRIVATE /tmp/fbcode_builder_getdeps-ZworkspacesZfollyZbuildZfbcode_builder-root/installed/folly/lib/libfolly.a)
