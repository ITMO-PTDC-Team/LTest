#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// The test might fail and that is expected under weak memory.
//
// Thread A performs a relaxed fetch_add on x (making x at least 1) and then
// a relaxed store to y. Thread B loads y then x and asserts it does not see
// y==1 while x still looks 0.
//
// Failure is allowed: relaxed operations on x and y impose no ordering between
// them across threads. B can observe y==1 (A's flag visible) while still
// reading a stale x==0, because B's load of x is not required to be ordered
// after its load of y, and there is no release/acquire (or seq_cst) edge
// tying visibility of the two locations together.

struct Exp11Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    x.fetch_add(1, std::memory_order_relaxed);
    y.store(1, std::memory_order_relaxed);
  }

  non_atomic void B() {
    int a = y.load(std::memory_order_relaxed);
    int b = x.load(std::memory_order_relaxed);
    rassert(!(a == 1 && b == 0));  // could fail
  }
};

struct Exp11Spec {
  using method_t = std::function<ValueWrapper(Exp11Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp11Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp11Test, Exp11Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp11Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp11Test, B),
                  }});
