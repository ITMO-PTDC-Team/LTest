#include "../specs/dual_deadlock.h"
#include "../../runtime/include/verifying.h"

// Always suspends and never resumes => all tasks block => scheduler deadlock.
struct DualDeadlockTarget {
  struct Awaitable {
    bool await_ready() noexcept { return false; }
    bool await_suspend(std::coroutine_handle<>) noexcept { return true; }
    int await_resume() noexcept { return 0; }
  };

  auto wait() noexcept { return Awaitable{}; }
};

target_method_dual(ltest::generators::genEmpty, int, DualDeadlockTarget, wait);

using spec_t = ltest::SpecDual<DualDeadlockTarget, spec::DualDeadlock>;
LTEST_ENTRYPOINT_DUAL(spec_t);