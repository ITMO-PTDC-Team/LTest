
#include <mutex>
#include <shared_mutex>

#include "../specs/register.h"
#include "runtime/include/blocking_primitives.h"
#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"

struct MutexedRegister {
 private:
  int x{};
  ltest::shared_mutex m{};

 public:
  non_atomic int add() {
    std::unique_lock lock{m};
    x++;
    return 0;
  }

  non_atomic int get() {
    std::shared_lock lock{m};
    return x;
  }

  void Reset() { x = 0; }
};

target_method(ltest::generators::genEmpty, int, MutexedRegister, add);

target_method(ltest::generators::genEmpty, int, MutexedRegister, get);

using spec_t =
    ltest::Spec<MutexedRegister, spec::LinearRegister, spec::LinearRegisterHash,
                spec::LinearRegisterEquals>;

LTEST_ENTRYPOINT(spec_t);