set(LIBCORO_SOURCE_TARGET_LIST
        libcoro_event.cpp
        libcoro_latch.cpp
        libcoro_mutex.cpp
        libcoro_queue.cpp
        libcoro_ring_buffer.cpp
        libcoro_semaphore.cpp
        libcoro_shared_mutex.cpp
)

set(LTEST_LIBCORO_SOURCE_DIR "/tmp/libcoro" CACHE PATH
        "Optional libcoro source checkout used by Docker dual images")

set(LTEST_LIBCORO_SOURCE_INCLUDE_DIR "")
if(EXISTS "${LTEST_LIBCORO_SOURCE_DIR}/include/coro/task.hpp")
    set(LTEST_LIBCORO_SOURCE_INCLUDE_DIR "${LTEST_LIBCORO_SOURCE_DIR}/include")
endif()

set(LTEST_LIBCORO_TARGET "")

if(LTEST_LIBCORO_SOURCE_INCLUDE_DIR)
    set(LIBCORO_INCLUDE_DIR "${LTEST_LIBCORO_SOURCE_INCLUDE_DIR}" CACHE PATH
            "libcoro include directory" FORCE)
    set(LIBCORO_BUILD_TESTS OFF CACHE BOOL "" FORCE)
    set(LIBCORO_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
    set(LIBCORO_FEATURE_NETWORKING OFF CACHE BOOL "" FORCE)
    set(LIBCORO_FEATURE_TLS OFF CACHE BOOL "" FORCE)
    set(LIBCORO_BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)

    if(NOT TARGET libcoro)
        add_subdirectory("${LTEST_LIBCORO_SOURCE_DIR}"
                "${CMAKE_BINARY_DIR}/_deps/libcoro-build"
                EXCLUDE_FROM_ALL)
    endif()

    set(LTEST_LIBCORO_TARGET libcoro)
else()
    find_path(LIBCORO_INCLUDE_DIR NAMES coro/task.hpp)

    find_library(LIBCORO_LIBRARY NAMES coro libcoro)
    if(LIBCORO_LIBRARY AND NOT EXISTS "${LIBCORO_LIBRARY}")
        unset(LIBCORO_LIBRARY CACHE)
        unset(LIBCORO_LIBRARY)
        find_library(LIBCORO_LIBRARY NAMES coro libcoro)
    endif()

    if(LIBCORO_LIBRARY)
        add_library(ltest_libcoro_external UNKNOWN IMPORTED)
        set_target_properties(ltest_libcoro_external PROPERTIES
                IMPORTED_LOCATION "${LIBCORO_LIBRARY}"
                INTERFACE_INCLUDE_DIRECTORIES "${LIBCORO_INCLUDE_DIR}"
        )
        set(LTEST_LIBCORO_TARGET ltest_libcoro_external)
    endif()
endif()

if(NOT LIBCORO_INCLUDE_DIR OR NOT LTEST_LIBCORO_TARGET)
    message(STATUS
            "Skipping libcoro dual targets: expected coro/task.hpp and either "
            "a libcoro library or a source checkout in LTEST_LIBCORO_SOURCE_DIR.")
    add_custom_target(verify-dual-libcoro)
    return()
endif()

set(LIBCORO_VERIFY_DUAL_LIST)

foreach(source_name ${LIBCORO_SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    verify_target(${target})
    target_link_libraries(${target} PRIVATE ${LTEST_LIBCORO_TARGET})
    list(APPEND LIBCORO_VERIFY_DUAL_LIST ${target})
endforeach()

add_custom_target(verify-dual-libcoro DEPENDS ${LIBCORO_VERIFY_DUAL_LIST})
