#include <mutex>

#include "folly/synchronization/Lock.h"
#include "runtime/include/verifying.h"
#include "verifying/specs/register.h"

typedef folly::detail::lock_base_unique<std::mutex> lock_guard;

struct Register {
  non_atomic void add() {
    lock_guard lock{m_};
    ++x_;
  }
  non_atomic int get() {
    lock_guard lock{m_};
    return x_;
  }

  void Reset() {
    lock_guard lock{m_};
    x_ = 0;
  }

  int x_{};
  std::mutex m_;
};

target_method(ltest::generators::genEmpty, void, Register, add);

target_method(ltest::generators::genEmpty, int, Register, get);

using spec_t =
    ltest::Spec<Register, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(spec_t);
