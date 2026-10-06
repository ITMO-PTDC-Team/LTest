find_path(FOLLY_INCLUDE_DIR NAMES folly/coro/Baton.h)
if(FOLLY_INCLUDE_DIR AND NOT EXISTS "${FOLLY_INCLUDE_DIR}/folly/coro/Baton.h")
    unset(FOLLY_INCLUDE_DIR CACHE)
    unset(FOLLY_INCLUDE_DIR)
    find_path(FOLLY_INCLUDE_DIR NAMES folly/coro/Baton.h)
endif()

find_library(FOLLY_LIBRARY NAMES folly)
if(FOLLY_LIBRARY AND NOT EXISTS "${FOLLY_LIBRARY}")
    unset(FOLLY_LIBRARY CACHE)
    unset(FOLLY_LIBRARY)
    find_library(FOLLY_LIBRARY NAMES folly)
endif()

find_path(GLOG_INCLUDE_DIR NAMES glog/logging.h)
find_path(EVENT_INCLUDE_DIR NAMES event2/event.h)
find_path(DOUBLE_CONVERSION_INCLUDE_DIR
        NAMES double-conversion/double-conversion.h)
find_library(GLOG_LIBRARY NAMES glog)
find_library(EVENT_LIBRARY NAMES event)
find_library(DOUBLE_CONVERSION_LIBRARY NAMES double-conversion)

if(NOT FOLLY_INCLUDE_DIR OR NOT FOLLY_LIBRARY OR
        NOT GLOG_INCLUDE_DIR OR NOT EVENT_INCLUDE_DIR OR
        NOT DOUBLE_CONVERSION_INCLUDE_DIR OR NOT GLOG_LIBRARY OR
        NOT EVENT_LIBRARY OR NOT DOUBLE_CONVERSION_LIBRARY OR
        NOT fmt_FOUND)
    message(STATUS
            "Skipping folly dual targets: expected folly headers/lib plus "
            "glog, libevent, double-conversion and fmt.")
    add_custom_target(verify-dual-folly)
    return()
endif()

set(FOLLY_SOURCE_TARGET_LIST
        folly_coro_baton.cpp
        folly_coro_bounded_queue.cpp
        folly_coro_mutex.cpp
        folly_coro_shared_promise.cpp
        folly_coro_shared_mutex.cpp
        folly_coro_unbounded_queue.cpp
)

set(FOLLY_VERIFY_DUAL_LIST)
set(FOLLY_NO_PLUGIN_TARGETS
        folly_coro_bounded_queue
        folly_coro_shared_promise
        folly_coro_unbounded_queue
)

foreach(source_name ${FOLLY_SOURCE_TARGET_LIST})
    get_filename_component(target ${source_name} NAME_WE)
    if(target IN_LIST FOLLY_NO_PLUGIN_TARGETS)
        verify_target_without_plugin(${target})
        target_compile_options(${target} PRIVATE -DNDEBUG)
    else()
        verify_target(${target})
    endif()
    target_link_options(${target} PRIVATE -Wl,--export-dynamic)
    target_include_directories(${target} PRIVATE
            ${FOLLY_INCLUDE_DIR}
            ${GLOG_INCLUDE_DIR}
            ${EVENT_INCLUDE_DIR}
            ${DOUBLE_CONVERSION_INCLUDE_DIR}
    )
    target_link_libraries(${target} PRIVATE
            ${GLOG_LIBRARY}
            ${FOLLY_LIBRARY}
            ${DOUBLE_CONVERSION_LIBRARY}
            fmt::fmt
            ${EVENT_LIBRARY}
    )
    list(APPEND FOLLY_VERIFY_DUAL_LIST ${target})
endforeach()

add_custom_target(verify-dual-folly DEPENDS ${FOLLY_VERIFY_DUAL_LIST})
