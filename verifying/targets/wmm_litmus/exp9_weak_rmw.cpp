#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// The test might fail and it is expected
struct Exp9Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    int expected = 0;
    while (!x.compare_exchange_strong(expected, 1, std::memory_order_relaxed,
                                      std::memory_order_relaxed)) {
      expected = 0;
    }
    y.store(1, std::memory_order_relaxed);
  }

  non_atomic void B() {
    int a = y.load(std::memory_order_relaxed);
    int b = x.load(std::memory_order_relaxed);
    assert(!(a == 1 && b == 0));  // could fail
  }
};

struct Exp9Spec {
  using method_t = std::function<ValueWrapper(Exp9Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp9Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t = ltest::Spec<Exp9Test, Exp9Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp9Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp9Test, B),
                  }});
