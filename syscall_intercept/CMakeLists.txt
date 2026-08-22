add_library(preload SHARED hook.cpp pthread.cpp resources.cpp)
add_library(preload_with_main SHARED hook.cpp pthread.cpp resources.cpp main.cpp)


set(PRELOAD_LIBS preload preload_with_main)

foreach(lib IN LISTS PRELOAD_LIBS)
    target_compile_options(${lib} PRIVATE "-fpic")
    target_link_libraries(${lib} PRIVATE syscall_intercept runtime)
    target_include_directories(${lib} PRIVATE ${CMAKE_SOURCE_DIR})
endforeach()
# target_link_options(preload PRIVATE ${CMAKE_ASAN_FLAGS})
# target_compile_options(preload PRIVATE ${CMAKE_ASAN_FLAGS})
