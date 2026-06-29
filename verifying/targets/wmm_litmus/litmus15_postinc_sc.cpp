#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Postfix x++ returns the previous value (0) and leaves x == 1. All seq_cst.
// B observes x in {0,1}. Should not fail under sound seq_cst.

struct Exp15Test {
  std::atomic<int> x{0};

  non_atomic void A() {
    int v = x++;
    rassert(v == 0);
    rassert(x.load(std::memory_order_seq_cst) == 1);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 0 && r <= 1);
  }
};

using spec_t = ltest::Spec<Exp15Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp15Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp15Test, B),
                  }});
