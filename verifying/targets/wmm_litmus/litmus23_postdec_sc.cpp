#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Postfix x-- returns the previous value (2) and leaves x == 1. All seq_cst.
// B observes x in {1,2}. Should not fail under sound seq_cst.

struct Exp23Test {
  std::atomic<int> x{2};

  non_atomic void A() {
    int v = x--;
    rassert(v == 2);
    rassert(x.load(std::memory_order_seq_cst) == 1);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 1 && r <= 2);
  }
};

struct Exp23Spec {
  using method_t = std::function<ValueWrapper(Exp23Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp23Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp23Test, Exp23Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp23Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp23Test, B),
                  }});
