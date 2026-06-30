//
// Created by bitree.
//
#include "../../specs/libcoro/mutex.h"
#include "../../blocking/verifiers/libcoro_mutex_verifier.h"

#include "../../../runtime/include/logger.h"
#include "../../../runtime/include/lib.h"
#include "../../../runtime/include/verifying.h"

#include <coro/mutex.hpp>
#include <coro/task.hpp>
#include <stdexcept>

struct LibcoroMutexTarget {
  coro::mutex m;

  LibcoroMutexTarget() : m() {}

  non_atomic auto lock() {
    return m.lock();
  }

  non_atomic bool try_lock() {
    return m.try_lock();
  }

  non_atomic void unlock() {
    try {
      m.unlock();
    } catch (const std::runtime_error&) {
    }
  }
};

target_method_dual(ltest::generators::genEmpty, void, LibcoroMutexTarget, lock);
// Keeping try_lock() in the public wrapper preserves the original API surface,
// but excluding it from the default workload keeps the historical lock/unlock
// bug benchmarks reproducible.
// target_method(ltest::generators::genEmpty, bool, LibcoroMutexTarget, try_lock);
target_method(ltest::generators::genEmpty, void, LibcoroMutexTarget, unlock);

using spec_t =
    ltest::SpecDual<LibcoroMutexTarget,
                    spec::LibcoroMutex,
                    spec::LibcoroMutexHash,
                    spec::LibcoroMutexEquals>;

LTEST_ENTRYPOINT_DUAL_CONSTRAINT(spec_t, LibcoroMutexVerifier);
