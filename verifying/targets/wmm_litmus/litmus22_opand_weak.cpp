#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// This test might fail and that is expected under weak memory.
//
// std::atomic<int>::operator&= only supports sequential consistency in the
// standard; there is no relaxed &=. We use fetch_and(memory_order_relaxed) to
// exercise the same AND-shaped RMW with relaxed semantics as in litmus11/17.
//
// Thread A: relaxed fetch_and(1) on x (starting 3 -> result 1), then relaxed
// store y=1. Thread B loads y then x and asserts !(y==1 && x==3) — i.e. if the
// flag is up, B should not still see the original x==3 after A's AND.
//
// How failure can occur: as with litmus11, relaxed operations on x and y are
// independent. B can read y==1 while still observing x==3 (stale read of x)
// because no acquire/release or seq_cst orders B's loads or ties y's store to
// x's RMW from B's viewpoint.

struct Exp22Test {
  std::atomic<int> x{3}, y{0};

  non_atomic void A() {
    x.fetch_and(1, std::memory_order_relaxed);
    y.store(1, std::memory_order_relaxed);
  }

  non_atomic void B() {
    int a = y.load(std::memory_order_relaxed);
    int b = x.load(std::memory_order_relaxed);
    rassert(!(a == 1 && b == 3));  // could fail
  }
};

using spec_t = ltest::Spec<Exp22Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp22Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp22Test, B),
                  }});
