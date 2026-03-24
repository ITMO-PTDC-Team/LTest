#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Release/acquire witness for fetch_xor. x starts at 1. A stores y=1 (relaxed)
// then fetch_xor(2, release), so x becomes 1^2==3. If B's acquire load sees
// x==3, release-acquire requires B to see y==1.

struct Exp18Test {
  std::atomic<int> x{1}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    int prev = x.fetch_xor(2, std::memory_order_release);
    rassert(prev == 1);
    rassert(x.load(std::memory_order_relaxed) == 3);
  }

  non_atomic void B() {
    int a = x.load(std::memory_order_acquire);
    int b = y.load(std::memory_order_relaxed);
    if (a == 3) {
      rassert(b == 1);  // must not fail if rel/acq is sound
    }
  }
};

struct Exp18Spec {
  using method_t = std::function<ValueWrapper(Exp18Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp18Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp18Test, Exp18Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp18Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp18Test, B),
                  }});
