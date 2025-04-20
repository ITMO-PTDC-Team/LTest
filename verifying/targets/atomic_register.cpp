/**
 * ./build/verifying/targets/atomic_register --tasks 3 --strategy tla --rounds 100000
 */
#include <atomic>

#include "../../runtime/include/verifying.h"
#include "../specs/register.h"

struct Register {
  NON_ATOMIC void Add() { x.fetch_add(1); }
  NON_ATOMIC int Get() { return x.load(); }

  void Reset() { x.store(0); }

  std::atomic<int> x{};
};

using SpecT =
    ltest::Spec<Register, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(ltest::generators::GenEmpty, void, Register, Add);

TARGET_METHOD(ltest::generators::GenEmpty, int, Register, Get);