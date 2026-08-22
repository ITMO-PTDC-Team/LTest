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

set(LTEST_DUAL_TEST_TIMEOUT "240" CACHE STRING
        "Timeout in seconds for each dual integration test")

function(add_dual_matrix_test group target variant)
    if(NOT TARGET ${target})
        return()
    endif()

    set(test_name "${target}_${variant}")
    add_integration_test_dual("${test_name}" "verify" FALSE
            ${target} ${ARGN} --deadlock_policy=explore
    )
    set_tests_properties("verify_${test_name}" PROPERTIES
            LABELS "verify;dual;${group}"
            TIMEOUT ${LTEST_DUAL_TEST_TIMEOUT}
    )
endfunction()

function(add_folly_default_matrix target)
    add_dual_matrix_test(dual-folly ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 40 --seed 291)
    add_dual_matrix_test(dual-folly ${target} pct_t2
            --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 292)
    add_dual_matrix_test(dual-folly ${target} weighted_t2
            --strategy random --threads 2 --weights 3,1 --tasks 4
            --rounds 30 --seed 293)
    add_dual_matrix_test(dual-folly ${target} rr_t2
            --strategy rr --threads 2 --tasks 4 --rounds 30 --seed 294)
    add_dual_matrix_test(dual-folly ${target} tla_t2
            --strategy tla --threads 2 --tasks 3 --switches 3 --rounds 10
            --seed 295)
    add_dual_matrix_test(dual-folly ${target} pct_min_t2
            --strategy pct --threads 2 --tasks 4 --rounds 20
            --minimize=true --exploration_runs=2 --minimization_runs=1
            --seed 301)
endfunction()

function(add_folly_shared_promise_matrix target)
    add_dual_matrix_test(dual-folly ${target} random_t1
            --strategy random --threads 1 --tasks 2 --rounds 5 --seed 321)
    add_dual_matrix_test(dual-folly ${target} pct_t1
            --strategy pct --threads 1 --tasks 2 --rounds 5 --seed 322)
    add_dual_matrix_test(dual-folly ${target} rr_t1
            --strategy rr --threads 1 --tasks 2 --rounds 5 --seed 323)
endfunction()

function(add_folly_shared_mutex_matrix target)
    add_dual_matrix_test(dual-folly ${target} weighted_t2
            --strategy random --threads 2 --weights 3,1 --tasks 4
            --rounds 20 --seed 401)
    add_dual_matrix_test(dual-folly ${target} pct_t3
            --strategy pct --threads 3 --tasks 6 --rounds 20 --seed 402)
    add_dual_matrix_test(dual-folly ${target} rr_t2
            --strategy rr --threads 2 --tasks 5 --rounds 30 --seed 411)
    add_dual_matrix_test(dual-folly ${target} tla_t2
            --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 20)
    add_dual_matrix_test(dual-folly ${target} pct_min_t3
            --strategy pct --threads 3 --tasks 6 --rounds 20
            --minimize=true --exploration_runs=4 --minimization_runs=2)
endfunction()

function(add_folly_bounded_queue_matrix target)
    add_dual_matrix_test(dual-folly ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 20 --seed 501)
    add_dual_matrix_test(dual-folly ${target} pct_t2
            --strategy pct --threads 2 --tasks 2 --rounds 5)
    add_dual_matrix_test(dual-folly ${target} rr_t2
            --strategy rr --threads 2 --tasks 4 --rounds 10)
    add_dual_matrix_test(dual-folly ${target} tla_t2
            --strategy tla --threads 2 --tasks 4 --switches 4 --rounds 5)
    add_dual_matrix_test(dual-folly ${target} random_min_t2
            --strategy random --threads 2 --tasks 4 --rounds 15
            --minimize=true --exploration_runs=3 --minimization_runs=2)
endfunction()

function(add_folly_mutex_matrix target)
    add_dual_matrix_test(dual-folly ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 30 --seed 311)
    add_dual_matrix_test(dual-folly ${target} pct_t2
            --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 312)
    add_dual_matrix_test(dual-folly ${target} rr_t2
            --strategy rr --threads 2 --tasks 4 --rounds 20 --seed 313)
    add_dual_matrix_test(dual-folly ${target} tla_t2
            --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 20)
    add_dual_matrix_test(dual-folly ${target} pct_min_t2
            --strategy pct --threads 2 --tasks 4 --rounds 15
            --minimize=true --exploration_runs=2 --minimization_runs=1
            --seed 314)
endfunction()

function(add_folly_unbounded_queue_matrix target)
    add_dual_matrix_test(dual-folly ${target} weighted_random_t2
            --strategy random --threads 2 --weights 5,1 --tasks 2 --rounds 5)
    add_dual_matrix_test(dual-folly ${target} random_t1
            --strategy random --threads 1 --tasks 2 --rounds 10)
    add_dual_matrix_test(dual-folly ${target} rr_t1
            --strategy rr --threads 1 --tasks 2 --rounds 10)
    add_dual_matrix_test(dual-folly ${target} tla_t2
            --strategy tla --threads 2 --tasks 2 --switches 4 --rounds 10)
    add_dual_matrix_test(dual-folly ${target} tla_min_t2
            --strategy tla --threads 2 --tasks 2 --switches 4 --rounds 5
            --minimize=true --exploration_runs=3 --minimization_runs=2)
endfunction()

function(add_libcoro_default_matrix target)
    add_dual_matrix_test(dual-libcoro ${target} random_t3
            --strategy random --threads 3 --tasks 8 --rounds 60)
    add_dual_matrix_test(dual-libcoro ${target} pct_t3
            --strategy pct --threads 3 --tasks 8 --rounds 40)
    add_dual_matrix_test(dual-libcoro ${target} weighted_t3
            --strategy random --threads 3 --weights 3,1,2 --tasks 8
            --rounds 40)
    add_dual_matrix_test(dual-libcoro ${target} rr_t2
            --strategy rr --threads 2 --tasks 6 --rounds 30)
    add_dual_matrix_test(dual-libcoro ${target} tla_t2
            --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 15)
    add_dual_matrix_test(dual-libcoro ${target} pct_min_t3
            --strategy pct --threads 3 --tasks 8 --rounds 30
            --minimize=true --exploration_runs=4 --minimization_runs=2)
endfunction()

function(add_libcoro_shared_mutex_matrix target)
    add_dual_matrix_test(dual-libcoro ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 20)
    add_dual_matrix_test(dual-libcoro ${target} weighted_t2
            --strategy random --threads 2 --weights 3,1 --tasks 4
            --rounds 20)
    add_dual_matrix_test(dual-libcoro ${target} random_t3
            --strategy random --threads 3 --tasks 6 --rounds 20)
    add_dual_matrix_test(dual-libcoro ${target} weighted_t3
            --strategy random --threads 3 --weights 3,1,2 --tasks 6
            --rounds 20)
    add_dual_matrix_test(dual-libcoro ${target} rr_t2
            --strategy rr --threads 2 --tasks 6 --rounds 30)
endfunction()

function(add_libcoro_mutex_matrix target)
    add_dual_matrix_test(dual-libcoro ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 40 --seed 203)
    add_dual_matrix_test(dual-libcoro ${target} pct_t2
            --strategy pct --threads 2 --tasks 5 --rounds 20 --seed 202)
    add_dual_matrix_test(dual-libcoro ${target} rr_t2
            --strategy rr --threads 2 --tasks 4 --rounds 20 --seed 204)
    add_dual_matrix_test(dual-libcoro ${target} random_min_t2
            --strategy random --threads 2 --tasks 4 --rounds 15
            --minimize=true --exploration_runs=2 --minimization_runs=1
            --seed 206)
endfunction()

function(add_libcoro_queue_matrix target)
    add_dual_matrix_test(dual-libcoro ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 40 --seed 111)
    add_dual_matrix_test(dual-libcoro ${target} pct_t2
            --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 112)
    add_dual_matrix_test(dual-libcoro ${target} rr_t2
            --strategy rr --threads 2 --tasks 4 --rounds 30 --seed 113)
    add_dual_matrix_test(dual-libcoro ${target} tla_t2
            --strategy tla --threads 2 --tasks 2 --switches 2 --rounds 5
            --seed 114)
    add_dual_matrix_test(dual-libcoro ${target} pct_min_t2
            --strategy pct --threads 2 --tasks 4 --rounds 15
            --minimize=true --exploration_runs=2 --minimization_runs=1
            --seed 115)
endfunction()

function(add_libcoro_semaphore_matrix target)
    add_dual_matrix_test(dual-libcoro ${target} random_t3
            --strategy random --threads 3 --tasks 8 --rounds 60)
    add_dual_matrix_test(dual-libcoro ${target} random_t2
            --strategy random --threads 2 --tasks 6 --rounds 40)
    add_dual_matrix_test(dual-libcoro ${target} rr_t2
            --strategy rr --threads 2 --tasks 6 --rounds 40)
    add_dual_matrix_test(dual-libcoro ${target} tla_t2
            --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 20)
    add_dual_matrix_test(dual-libcoro ${target} random_min_t3
            --strategy random --threads 3 --tasks 8 --rounds 30
            --minimize=true --exploration_runs=4 --minimization_runs=2)
endfunction()

function(add_libcoro_ring_buffer_matrix target)
    add_dual_matrix_test(dual-libcoro ${target} random_t2
            --strategy random --threads 2 --tasks 4 --rounds 40 --seed 121)
    add_dual_matrix_test(dual-libcoro ${target} pct_t2
            --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 122)
    add_dual_matrix_test(dual-libcoro ${target} weighted_t2
            --strategy random --threads 2 --weights 3,1 --tasks 4
            --rounds 20 --seed 123)
    add_dual_matrix_test(dual-libcoro ${target} rr_t2
            --strategy rr --threads 2 --tasks 4 --rounds 30 --seed 124)
    add_dual_matrix_test(dual-libcoro ${target} tla_t2
            --strategy tla --threads 2 --tasks 2 --switches 2 --rounds 5
            --seed 125)
    add_dual_matrix_test(dual-libcoro ${target} pct_min_t2
            --strategy pct --threads 2 --tasks 4 --rounds 15
            --minimize=true --exploration_runs=2 --minimization_runs=1
            --seed 126)
endfunction()

add_subdirectory(libcoro)
add_subdirectory(folly)

if(VK_DUAL_AVAILABLE)
    add_subdirectory(vk)
else()
    add_custom_target(verify-dual-vk)
endif()

add_dual_matrix_test(dual-core coro_async_stack pct
        --strategy pct --rounds 1000)

add_libcoro_default_matrix(libcoro_event)
add_libcoro_default_matrix(libcoro_latch)
add_libcoro_mutex_matrix(libcoro_mutex)
add_libcoro_queue_matrix(libcoro_queue)
add_libcoro_ring_buffer_matrix(libcoro_ring_buffer)
add_libcoro_semaphore_matrix(libcoro_semaphore)
add_libcoro_shared_mutex_matrix(libcoro_shared_mutex)

add_folly_default_matrix(folly_coro_baton)
add_folly_bounded_queue_matrix(folly_coro_bounded_queue)
add_folly_mutex_matrix(folly_coro_mutex)
add_folly_shared_mutex_matrix(folly_coro_shared_mutex)
add_folly_shared_promise_matrix(folly_coro_shared_promise)
add_folly_unbounded_queue_matrix(folly_coro_unbounded_queue)

add_custom_target(verify-dual DEPENDS
        ${VERIFY_DUAL_LIST}
        verify-dual-libcoro
        verify-dual-folly
        verify-dual-vk
)
