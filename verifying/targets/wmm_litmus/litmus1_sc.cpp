#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp1Test {
  std::atomic<int> x{0}, y{0};
  int r1 = -1, r2 = -1;

  non_atomic void A() {
    r1 = y.load(std::memory_order_seq_cst);
    x.store(1, std::memory_order_seq_cst);
    rassert(!(r1 == 1 && r2 == 1));
  }

  non_atomic void B() {
    r2 = x.load(std::memory_order_seq_cst);
    y.store(1, std::memory_order_seq_cst);
    rassert(!(r1 == 1 && r2 == 1));
  }
};

using spec_t =
    ltest::Spec<Exp1Test, LitmusTwoThreadsSpec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp1Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp1Test, B),
                  }});
