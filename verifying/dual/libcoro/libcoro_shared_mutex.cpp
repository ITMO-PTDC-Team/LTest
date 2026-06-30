//
// Created by bitree.
//

#include "../../specs/libcoro/shared_mutex.h"
#include "../../blocking/verifiers/libcoro_shared_mutex_verifier.h"

#include "../../../runtime/include/verifying.h"

#include <coroutine>
#include <memory>
#include <utility>

#include <coro/shared_mutex.hpp>
#include <coro/task.hpp>

struct InlineExecutor {
  struct ImmediateAwaiter {
    bool await_ready() const noexcept { return true; }
    void await_suspend(std::coroutine_handle<>) const noexcept {}
    void await_resume() const noexcept {}
  };

  auto schedule() noexcept -> ImmediateAwaiter {
    return {};
  }

  auto yield() noexcept -> ImmediateAwaiter {
    return {};
  }

  bool spawn_detached(coro::task<void> t) noexcept {
    (void)t;
    return true;
  }

  coro::task<void> spawn_joinable(coro::task<void> t) noexcept {
    return std::move(t);
  }

  bool resume(std::coroutine_handle<> h) noexcept {
    if (h) {
      h.resume();
      return true;
    }
    return false;
  }

  std::size_t size() const noexcept {
    return 0;
  }

  bool empty() const noexcept {
    return true;
  }

  void shutdown() noexcept {}
};

struct LibcoroSharedMutexTarget {
  std::unique_ptr<InlineExecutor> ex;
  coro::shared_mutex<InlineExecutor> m;

  LibcoroSharedMutexTarget()
      : ex(std::make_unique<InlineExecutor>()), m(ex) {}

  non_atomic auto lock() { return m.lock(); }

  non_atomic auto lock_shared() { return m.lock_shared(); }

  non_atomic auto unlock() { return m.unlock(); }

  non_atomic auto unlock_shared() { return m.unlock_shared(); }
};

target_method_dual(ltest::generators::genEmpty, void,
                   LibcoroSharedMutexTarget, lock);

target_method_dual(ltest::generators::genEmpty, void,
                   LibcoroSharedMutexTarget, lock_shared);

target_method_async(ltest::generators::genEmpty, void,
                    LibcoroSharedMutexTarget, unlock);

target_method_async(ltest::generators::genEmpty, void,
                    LibcoroSharedMutexTarget, unlock_shared);

using spec_t =
    ltest::SpecDual<LibcoroSharedMutexTarget,
                    spec::LibcoroSharedMutex,
                    spec::LibcoroSharedMutexHash,
                    spec::LibcoroSharedMutexEquals>;

LTEST_ENTRYPOINT_DUAL_CONSTRAINT(spec_t, LibcoroSharedMutexVerifier);
