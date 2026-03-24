#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// This test might fail and that is expected under weak memory.
//
// operator^= on atomic has no relaxed form; fetch_xor(relaxed) models XOR-RMW
// with the same visibility pitfall as litmus11/17/22/24.
//
// x starts at 3. A applies fetch_xor(1, relaxed) -> 2, then y.store(1, relaxed).
// B loads y then x and forbids y==1 with x still 3 (unchanged from B's view).
//
// How failure can occur: B can observe the flag y==1 while still reading the
// stale value x==3 because relaxed accesses do not enforce cross-location
// ordering between B's loads or between A's RMW and store as seen by B.

struct Exp26Test {
  std::atomic<int> x{3}, y{0};

  non_atomic void A() {
    x.fetch_xor(1, std::memory_order_relaxed);
    y.store(1, std::memory_order_relaxed);
  }

  non_atomic void B() {
    int a = y.load(std::memory_order_relaxed);
    int b = x.load(std::memory_order_relaxed);
    rassert(!(a == 1 && b == 3));  // could fail
  }
};

struct Exp26Spec {
  using method_t = std::function<ValueWrapper(Exp26Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp26Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp26Test, Exp26Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp26Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp26Test, B),
                  }});
