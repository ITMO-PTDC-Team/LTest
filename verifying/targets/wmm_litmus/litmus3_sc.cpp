#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp3Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(20, std::memory_order_seq_cst);
    x.store(10, std::memory_order_seq_cst);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_seq_cst) == 10) {
      rassert(y.load(std::memory_order_seq_cst) == 20);
      y.store(10, std::memory_order_seq_cst);
    }
  }

  non_atomic void C() {
    if (y.load(std::memory_order_seq_cst) == 10) {
      rassert(x.load(std::memory_order_seq_cst) == 10);
    }
  }
};

using spec_t = ltest::Spec<Exp3Test, LitmusThreeThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp3Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp3Test, B),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp3Test, C),
                  }});
