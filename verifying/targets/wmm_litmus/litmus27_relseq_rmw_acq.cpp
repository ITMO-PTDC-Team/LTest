#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Release sequence through a relaxed RMW (two threads).
//
// T1: relaxed store to y, then release store to x (x becomes 1 from 0).
// T2: relaxed fetch_add(1) on x (RMW: reads 1, publishes 2 — still in the
//     release sequence headed by T1's release). Then acquire load of x.
//
// If the acquire observes x == 2, it reads-from the RMW modification, which
// lies in the release sequence of T1's release store. Then synchronize-with
// holds and T1's relaxed store to y is happens-before-visible; y must read
// as 1.
//
// expect_fail FALSE if release sequences + sw are implemented soundly.

struct Exp27Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    x.store(1, std::memory_order_release);
  }

  non_atomic void B() {
    x.fetch_add(1, std::memory_order_relaxed);
    int r = x.load(std::memory_order_acquire);
    int yy = y.load(std::memory_order_relaxed);
    if (r == 2) {
      // Release sequence if established, so this thread syncs with T1
      rassert(yy == 1);  // should never fail
    }
  }
};

struct Exp27Spec {
  using method_t = std::function<ValueWrapper(Exp27Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp27Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp27Test, Exp27Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp27Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp27Test, B),
                  }});
