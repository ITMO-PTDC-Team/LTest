#include <mutex>

#include "folly/synchronization/Lock.h"
#include "runtime/include/verifying.h"
#include "verifying/specs/register.h"

using lock_guard = folly::detail::lock_base_unique<std::mutex> ;

struct Register {
  NON_ATOMIC void Add() {
    lock_guard lock{m_};
    ++x_;
  }
  NON_ATOMIC int Get() {
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

using SpecT =
    ltest::Spec<Register, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(ltest::generators::GenEmpty, void, Register, Add);

TARGET_METHOD(ltest::generators::GenEmpty, int, Register, Get);