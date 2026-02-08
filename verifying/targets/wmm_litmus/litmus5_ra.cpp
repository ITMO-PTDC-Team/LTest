#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp5Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(20, std::memory_order_release);
    x.store(10, std::memory_order_release);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_acquire) == 10) {
      rassert(y.load(std::memory_order_acquire) == 20);
      y.store(10, std::memory_order_release);
    }
  }

  non_atomic void C() {
    if (y.load(std::memory_order_acquire) == 10) {
      rassert(x.load(std::memory_order_acquire) == 10);
    }
  }
};

struct Exp5Spec {
  using method_t = std::function<ValueWrapper(Exp5Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp5Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
        {"C", func},
    };
  }
};

using spec_t = ltest::Spec<Exp5Test, Exp5Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp5Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp5Test, B),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp5Test, C),
                  }});
