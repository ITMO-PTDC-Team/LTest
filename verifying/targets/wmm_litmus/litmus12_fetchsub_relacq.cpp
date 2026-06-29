#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// This litmus test checks the release/acquire ordering guarantees in C++
// atomics. Thread A first performs a relaxed store to y, setting y = 1, and
// then does a release fetch_sub of 2 on x (initially x = 1). Thread B performs
// an acquire load of x and a relaxed load of y. If B observes x == -1 (meaning
// it saw the effect of A's fetch_sub), then it must also see y == 1 due to the
// release and acquire synchronization (i.e., the memory_order_release operation
// on A synchronizes-with the memory_order_acquire load in B if the modification
// is observed), enforcing happens-before on A's relaxed store to y. This test
// must not fail the assertion in B if the model correctly implements
// release/acquire semantics.

struct Exp12Test {
  std::atomic<int> x{1}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    x.fetch_sub(2, std::memory_order_release);
  }

  non_atomic void B() {
    int a = x.load(std::memory_order_acquire);
    int b = y.load(std::memory_order_relaxed);
    if (a == -1) {
      rassert(b == 1);  // must not fail
    }
  }
};

using spec_t = ltest::Spec<Exp12Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp12Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp12Test, B),
                  }});
