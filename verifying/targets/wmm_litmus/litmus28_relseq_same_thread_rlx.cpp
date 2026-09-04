#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Release sequence: same-thread relaxed store after the release head, then RMW.
//
// T1: relaxed y; release store to x (0 -> 1); final relaxed store to x (1 ->
// 100).
//     The last store is on the same thread as the release head, so it is part
//     of the release sequence headed by the release write.
// T2: relaxed fetch_add(5) on x — the RMW should read-from T1's last relaxed
//     write (100), producing 105. Then acquire load of x.
//
// If the acquire sees x == 105, it reads-from the RMW; that RMW reads-from a
// modification still in RS(release), so synchronize-with still links T1's
// release to the acquire and T1's relaxed store to y must be visible (y == 1).
//
// expect_fail FALSE if release sequences include same-thread relaxed stores
// after the head and RMW extensions are wired correctly.

struct Exp28Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    x.store(1, std::memory_order_release);
    x.store(100, std::memory_order_relaxed);
  }

  non_atomic void B() {
    x.fetch_add(5, std::memory_order_relaxed);
    int r = x.load(std::memory_order_acquire);
    int yy = y.load(std::memory_order_relaxed);
    if (r == 105) {
      rassert(yy == 1);  // should never fail
    }
  }
};

struct Exp28Spec {
  using method_t = std::function<ValueWrapper(Exp28Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp28Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp28Test, Exp28Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp28Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp28Test, B),
                  }});
