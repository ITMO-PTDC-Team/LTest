#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp2Test {
  std::atomic<int> x{0}, y{0};
  int r1 = -1;

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    x.store(2, std::memory_order_seq_cst);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_seq_cst) == 2) {
      r1 = y.load(std::memory_order_relaxed);
      rassert(r1 == 1);
    }
  }
};

using spec_t =
    ltest::Spec<Exp2Test, LitmusTwoThreadsSpec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp2Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp2Test, B),
                  }});
