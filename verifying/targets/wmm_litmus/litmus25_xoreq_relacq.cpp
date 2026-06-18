#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// std::atomic::operator^= is seq_cst-only; we use fetch_xor(release) with an
// acquire load witness (same pattern as litmus16/18).
//
// x starts at 5 (0b101). A stores y=1 relaxed, then fetch_xor(3, release)
// giving 5^3==6. If B's acquire load sees x==6, it must see y==1.

struct Exp25Test {
  std::atomic<int> x{5}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    int prev = x.fetch_xor(3, std::memory_order_release);
    rassert(prev == 5);
    rassert(x.load(std::memory_order_relaxed) == 6);
  }

  non_atomic void B() {
    int a = x.load(std::memory_order_acquire);
    int b = y.load(std::memory_order_relaxed);
    if (a == 6) {
      rassert(b == 1);  // must not fail if rel/acq is sound
    }
  }
};

using spec_t = ltest::Spec<Exp25Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp25Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp25Test, B),
                  }});
