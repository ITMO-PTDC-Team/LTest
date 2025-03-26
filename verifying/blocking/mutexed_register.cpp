#include <mutex>

#include "runtime/include/verifying.h"
#include "verifying/specs/register.h"

struct Register {
  non_atomic void add() {
    std::lock_guard lock{m_};
    ++x_;
  }
  non_atomic int get() {
    std::lock_guard lock{m_};
    return x_;
  }

  void Reset() {
    std::lock_guard lock{m_};
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
