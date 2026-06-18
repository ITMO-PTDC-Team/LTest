#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Release/acquire witness for fetch_and (same idea as litmus12 with fetch_sub).
// x starts at 3 (0b11). A stores y=1 (relaxed) then fetch_and(1, release),
// leaving x == 1. If B's acquire load sees x == 1, it must see y == 1
// (release on the RMW synchronizes-with acquire load when B observes the
// write).

struct Exp16Test {
  std::atomic<int> x{3}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    int prev = x.fetch_and(1, std::memory_order_release);
    rassert(prev == 3);
    rassert(x.load(std::memory_order_relaxed) == 1);
  }

  non_atomic void B() {
    int a = x.load(std::memory_order_acquire);
    int b = y.load(std::memory_order_relaxed);
    if (a == 1) {
      rassert(b == 1);  // must not fail if rel/acq is sound
    }
  }
};

using spec_t = ltest::Spec<Exp16Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp16Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp16Test, B),
                  }});
