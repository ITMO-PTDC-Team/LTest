#include "../../specs/libcoro/semaphore.h"

#include "../../../runtime/include/verifying.h"

#include <coro/semaphore.hpp>

struct LibcoroSemaphoreAcquireAwaiter {
  using Task = coro::task<coro::semaphore_acquire_result>;
  using Inner = decltype(std::declval<Task&&>().operator co_await());

  explicit LibcoroSemaphoreAcquireAwaiter(Task t)
      : task(std::move(t)),
        inner(std::move(task).operator co_await()) {}

  as_atomic bool await_ready() noexcept(noexcept(inner.await_ready())) {
    return inner.await_ready();
  }

  as_atomic decltype(auto) await_suspend(std::coroutine_handle<> h) noexcept(
      noexcept(inner.await_suspend(h))) {
    return inner.await_suspend(h);
  }

  as_atomic decltype(auto) await_resume() noexcept(
      noexcept(inner.await_resume())) {
    return inner.await_resume();
  }

  Task task;
  Inner inner;
};

struct LibcoroSemaphoreTarget {
  static constexpr std::ptrdiff_t kMax = spec::LibcoroSemaphore::kMax;
  static constexpr std::ptrdiff_t kInitial = spec::LibcoroSemaphore::kInitial;

  coro::semaphore<kMax> sem{kInitial};

  non_atomic auto acquire() {
    return LibcoroSemaphoreAcquireAwaiter{sem.acquire()};
  }

  non_atomic auto release() {
    return sem.release();
  }

  non_atomic auto shutdown() {
    return sem.shutdown();
  }

  non_atomic bool try_acquire() {
    return sem.try_acquire();
  }

  non_atomic std::ptrdiff_t value() {
    return sem.value();
  }

  non_atomic std::ptrdiff_t max() {
    return sem.max();
  }

  non_atomic bool is_shutdown() {
    return sem.is_shutdown();
  }
};

target_method_dual(ltest::generators::genEmpty, coro::semaphore_acquire_result,
                   LibcoroSemaphoreTarget, acquire);
target_method_async(ltest::generators::genEmpty, void,
                    LibcoroSemaphoreTarget, release);
target_method_async(ltest::generators::genEmpty, void,
                    LibcoroSemaphoreTarget, shutdown);
target_method(ltest::generators::genEmpty, bool,
              LibcoroSemaphoreTarget, try_acquire);
target_method(ltest::generators::genEmpty, std::ptrdiff_t,
              LibcoroSemaphoreTarget, value);
target_method(ltest::generators::genEmpty, std::ptrdiff_t,
              LibcoroSemaphoreTarget, max);
target_method(ltest::generators::genEmpty, bool,
              LibcoroSemaphoreTarget, is_shutdown);

using spec_t =
    ltest::SpecDual<LibcoroSemaphoreTarget,
                    spec::LibcoroSemaphore,
                    spec::LibcoroSemaphoreHash,
                    spec::LibcoroSemaphoreEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
