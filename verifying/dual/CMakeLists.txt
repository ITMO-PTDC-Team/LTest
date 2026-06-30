# Local eng archive root
set(ENG_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/eng)
set(VK_DUAL_AVAILABLE FALSE)

find_package(fmt QUIET)
find_path(LTEST_FUNCTION2_INCLUDE_DIR NAMES function2/function2.hpp)
if(LTEST_FUNCTION2_INCLUDE_DIR AND
        NOT EXISTS "${LTEST_FUNCTION2_INCLUDE_DIR}/function2/function2.hpp")
    unset(LTEST_FUNCTION2_INCLUDE_DIR CACHE)
    unset(LTEST_FUNCTION2_INCLUDE_DIR)
    find_path(LTEST_FUNCTION2_INCLUDE_DIR NAMES function2/function2.hpp)
endif()

if(EXISTS "${ENG_ROOT}/net/net2/fibers" AND
        EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/vk/CMakeLists.txt" AND
        LTEST_FUNCTION2_INCLUDE_DIR AND
        fmt_FOUND)
    set(VK_DUAL_AVAILABLE TRUE)
    include_directories(${ENG_ROOT})

# ------------------------------------------------------------------------------
# Minimal eng runtime slice required by vk::fibers targets
# ------------------------------------------------------------------------------
file(GLOB ENG_FIBERS_SCHED_CPP CONFIGURE_DEPENDS
        ${ENG_ROOT}/net/net2/fibers/scheduler/*.cpp
)

file(GLOB ENG_FIBERS_INTERNAL_CPP CONFIGURE_DEPENDS
        ${ENG_ROOT}/net/net2/fibers/internal/*.cpp
)
list(FILTER ENG_FIBERS_INTERNAL_CPP EXCLUDE REGEX ".*test\\.cpp$")

file(GLOB ENG_FIBERS_MR_CPP CONFIGURE_DEPENDS
        ${ENG_ROOT}/net/net2/fibers/memory-reclamation/*.cpp
)
list(FILTER ENG_FIBERS_MR_CPP EXCLUDE REGEX ".*test\\.cpp$")

file(GLOB ENG_FIBERS_SP_INTERNAL_CPP CONFIGURE_DEPENDS
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/internal/*.cpp
)
list(FILTER ENG_FIBERS_SP_INTERNAL_CPP EXCLUDE REGEX ".*test\\.cpp$")

set(ENG_FIBERS_SP_CPP
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/select.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/channel.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/mutex.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/rw-mutex.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/semaphore.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/weighted-semaphore.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/cond-var.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/event-count.cpp
        ${ENG_ROOT}/net/net2/fibers/sync-primitives/channel.cpp
)

# Генерируем файл-заглушку для всех недостающих символов
set(ENG_STUBS_CPP ${CMAKE_CURRENT_BINARY_DIR}/vk_eng_stubs.cpp)
file(WRITE ${ENG_STUBS_CPP} "// Заглушки для символов, требуемых fibers
#include <fmt/core.h>
#include <fmt/args.h>
#include <functional>
#include <string_view>
#include <cstdint>

namespace vk {
enum class LogLevel {};
namespace detail {
struct source_location {};
void vklog_raw(LogLevel, source_location, fmt::string_view, fmt::format_args) {}
} // namespace detail
} // namespace vk

void init_time() {}
void async_print_buffer(const char*, int) {}

struct option_section_t {};
void parse_common_option(option_section_t, int (*)(int), const char*, int, int, const char*, ...) {}
long long parse_memory_limit(const char*) { return 0; }
double get_utime_monotonic() { return 0.0; }

int verbosity = 0;
int tornado_debug = 0;
std::function<void(int*, std::string_view, vk::LogLevel)> g_pre_log_callback{};

void register_verbosity_type_inner(std::string_view, int32_t*) {}
")

add_library(vk_eng_runtime STATIC
        ${ENG_FIBERS_SCHED_CPP}
        ${ENG_FIBERS_INTERNAL_CPP}
        ${ENG_FIBERS_MR_CPP}
        ${ENG_FIBERS_SP_INTERNAL_CPP}
        ${ENG_FIBERS_SP_CPP}
        ${ENG_ROOT}/net/net2/fibers/internal/sync/futex.cpp
        ${ENG_STUBS_CPP}
)

target_include_directories(vk_eng_runtime PUBLIC
        ${ENG_ROOT}
        ${LTEST_FUNCTION2_INCLUDE_DIR}
)
target_link_libraries(vk_eng_runtime PUBLIC fmt::fmt)
target_compile_features(vk_eng_runtime PUBLIC cxx_std_20)
target_compile_options(vk_eng_runtime PRIVATE -Wno-deprecated-declarations)
else()
    message(STATUS
            "Skipping local vk dual targets: expected verifying/dual/eng, "
            "verifying/dual/vk, function2/function2.hpp and fmt.")
endif()

# ------------------------------------------------------------------------------
# Dual verification targets (non-vk)
# ------------------------------------------------------------------------------
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
    set(full_name "${label}_${test_name}")
    add_integration_test(${test_name} ${label} ${fail} ${ARGN})
endfunction()

add_subdirectory(libcoro)
add_subdirectory(folly)

if(VK_DUAL_AVAILABLE)
    add_subdirectory(vk)
else()
    add_custom_target(verify-dual-vk)
endif()

add_integration_test_dual("coro_async_stack_pct" "verify" FALSE
        coro_async_stack --strategy pct --rounds 1000 --deadlock_policy=explore
)

if(TARGET libcoro_queue)
    add_integration_test_dual("libcoro_queue_pct" "verify" FALSE
            libcoro_queue --strategy pct --rounds 1000 --deadlock_policy=explore
    )
endif()

if(TARGET folly_coro_mutex)
    add_integration_test_dual("folly_coro_mutex_pct" "verify" FALSE
            folly_coro_mutex --strategy pct --rounds 1000 --deadlock_policy=explore
    )
endif()

if(TARGET folly_coro_baton)
    add_integration_test_dual("folly_coro_baton_pct" "verify" FALSE
            folly_coro_baton --strategy pct --rounds 1000 --deadlock_policy=explore
    )
endif()

if(TARGET folly_coro_shared_mutex)
    add_integration_test_dual("folly_coro_shared_mutex_pct" "verify" FALSE
            folly_coro_shared_mutex --strategy pct --rounds 1000 --deadlock_policy=explore
    )
endif()

add_custom_target(verify-dual DEPENDS
        ${VERIFY_DUAL_LIST}
        verify-dual-libcoro
        verify-dual-folly
        verify-dual-vk
)
