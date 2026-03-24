#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// This test might fail and that is expected under weak memory.
//
// std::atomic::operator|= has no relaxed overload; we use fetch_or(relaxed) to
// model the same OR-shaped RMW with weak ordering as litmus17.
//
// Thread A: relaxed fetch_or(4) on x (1|4 -> 5), then relaxed y=1. Thread B
// loads y then x and asserts it does not see y==1 while x is still 1 (A's OR
// not yet visible to B).
//
// How failure can occur: relaxed RMW on x and relaxed store on y are not
// synchronized across threads. B may observe y==1 while x still reads 1,
// matching litmus11/17 style reordering of observations.

struct Exp24Test {
  std::atomic<int> x{1}, y{0};

  non_atomic void A() {
    x.fetch_or(4, std::memory_order_relaxed);
    y.store(1, std::memory_order_relaxed);
  }

  non_atomic void B() {
    int a = y.load(std::memory_order_relaxed);
    int b = x.load(std::memory_order_relaxed);
    rassert(!(a == 1 && b == 1));  // could fail (stale x before OR visible)
  }
};

struct Exp24Spec {
  using method_t = std::function<ValueWrapper(Exp24Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp24Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp24Test, Exp24Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp24Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp24Test, B),
                  }});
