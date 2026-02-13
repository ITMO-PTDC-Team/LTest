#include <atomic>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// The test might fail and it is expected
struct Exp4Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(20, std::memory_order_relaxed);
    x.store(10, std::memory_order_relaxed);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_relaxed) == 10) {
      rassert(y.load(std::memory_order_relaxed) == 20);  // could fail
      y.store(10, std::memory_order_relaxed);
    }
  }

  non_atomic void C() {
    if (y.load(std::memory_order_relaxed) == 10) {
      rassert(x.load(std::memory_order_relaxed) == 10);  // could fail
    }
  }
};

struct Exp4Spec {
  using method_t = std::function<ValueWrapper(Exp4Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp4Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
        {"C", func},
    };
  }
};

using spec_t = ltest::Spec<Exp4Test, Exp4Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp4Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp4Test, B),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp4Test, C),
                  }});
