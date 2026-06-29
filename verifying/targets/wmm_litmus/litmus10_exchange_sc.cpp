#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Seq-cst only exchange: thread A replaces x=0 with x=1 via
// exchange and checks it saw the initial value. Thread B only observes x in
// {0,1}, and if it was 1, then checks for visibility of y=1 from relaxed write.

struct Exp10Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    int old = x.exchange(1, std::memory_order_seq_cst);
    rassert(old == 0);
    rassert(x.load(std::memory_order_seq_cst) == 1);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 0 && r <= 1);
    if (r == 1) {
      rassert(y.load(std::memory_order_relaxed) == 1);  // must not fail
    }
  }
};

using spec_t = ltest::Spec<Exp10Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp10Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp10Test, B),
                  }});
