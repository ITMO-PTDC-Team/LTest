#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// operator|= with seq_cst. x starts at 1; A ORs in 4 to get 5. B observes
// x in {1,5}. Should not fail under seq_cst.

struct Exp20Test {
  std::atomic<int> x{1};

  non_atomic void A() {
    int v = (x |= 4);
    rassert(v == 5);
    rassert(x.load(std::memory_order_seq_cst) == 5);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r == 1 || r == 5);
  }
};

struct Exp20Spec {
  using method_t = std::function<ValueWrapper(Exp20Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp20Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp20Test, Exp20Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp20Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp20Test, B),
                  }});
