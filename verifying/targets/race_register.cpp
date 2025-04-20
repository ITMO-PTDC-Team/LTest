#include "runtime/include/verifying.h"
#include "verifying/specs/register.h"

struct Register {
  NON_ATOMIC void Add() { ++x; }

  NON_ATOMIC int Get() { return x; }

  void Reset() { x = 0; }

  int x{};
};

using SpecT =
    ltest::Spec<Register, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(ltest::generators::GenEmpty, void, Register, Add);

TARGET_METHOD(ltest::generators::GenEmpty, int, Register, Get);