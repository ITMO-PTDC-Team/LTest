#include "../../runtime/include/verifying.h"
#include "../specs/register.h"

struct Register {
  void add();
  int get();

  Register(const Register& oth) { x = oth.x; }
  Register& operator=(const Register& oth) {
    x = oth.x;
    return *this;
  }
  Register(){};

  int x{};
};

target_method(ltest::generators::empty_gen, void, Register, add) { ++x; }

target_method(ltest::generators::empty_gen, int, Register, get) { return x; }

using spec_t =
    ltest::Spec<Register, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(spec_t);
