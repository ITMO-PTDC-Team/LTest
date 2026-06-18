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
    rassert(!(a == 1 && b == 0));  // could fail
  }
};

using spec_t =
    ltest::Spec<Exp9Test, LitmusTwoThreadsSpec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp9Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp9Test, B),
                  }});
