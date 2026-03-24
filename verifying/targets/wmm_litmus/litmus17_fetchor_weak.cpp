#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// This test might fail and that is expected under weak memory (cf. litmus11).
//
// Thread A: relaxed store y=1 first, then relaxed fetch_or(1) on x (0 -> 1).
// Thread B: relaxed load of x first, then relaxed load of y. We forbid
// observing y==0 while x reads 1.
//
// How failure can occur: even though A sequences y before the x RMW in program
// order, both are relaxed, so there is no release/acquire edge to B. B's two
// relaxed loads are unordered relative to each other. B can read x==1
// and then read y==0.

struct Exp17Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    x.fetch_or(1, std::memory_order_relaxed);
  }

  non_atomic void B() {
    int rx = x.load(std::memory_order_relaxed);
    int ry = y.load(std::memory_order_relaxed);
    rassert(!(rx == 1 && ry == 0));  // could fail
  }
};

struct Exp17Spec {
  using method_t = std::function<ValueWrapper(Exp17Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp17Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp17Test, Exp17Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp17Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp17Test, B),
                  }});
