#include "runtime/include/verifying.h"
#include "verifying/specs/register.h"

struct Register {
  non_atomic void add() { ++x; }

  non_atomic int get() { return x; }

  int x{};
};

using spec_t =
    ltest::Spec<Register, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(spec_t);

target_method(ltest::generators::genEmpty, void, Register, add);

target_method(ltest::generators::genEmpty, int, Register, get);
