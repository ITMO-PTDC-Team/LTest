#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Prefix -- with seq_cst: x starts at 1. A decrements to 0 and checks the
// value returned by --x. B observes x in {0,1}. Should not fail under sound
// seq_cst.

struct Exp14Test {
  std::atomic<int> x{1};

  non_atomic void A() {
    int v = --x;
    rassert(v == 0);
    rassert(x.load(std::memory_order_seq_cst) == 0);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 0 && r <= 1);
  }
};

struct Exp14Spec {
  using method_t = std::function<ValueWrapper(Exp14Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp14Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp14Test, Exp14Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp14Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp14Test, B),
                  }});
