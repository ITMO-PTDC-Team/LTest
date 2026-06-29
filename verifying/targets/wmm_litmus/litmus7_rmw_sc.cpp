#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp7Test {
  std::atomic<int> x{0};

  non_atomic void A() {
    int expected = 0;
    do {
      expected = x.load(std::memory_order_seq_cst);
    } while (!x.compare_exchange_weak(expected, expected + 1,
                                      std::memory_order_seq_cst));
    rassert(expected == 0);
    rassert(x.load(std::memory_order_seq_cst) == 1);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 0 && r <= 1);
  }
};

using spec_t =
    ltest::Spec<Exp7Test, LitmusTwoThreadsSpec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp7Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp7Test, B),
                  }});
