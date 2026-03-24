#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// operator&= on std::atomic<int> uses sequential consistency. x starts at 6;
// A clears high bits with &= 3, yielding 2. B observes x in {2,6} depending
// on interleaving. Should not fail under seq_cst.

struct Exp19Test {
  std::atomic<int> x{6};

  non_atomic void A() {
    int v = (x &= 3);
    rassert(v == 2);
    rassert(x.load(std::memory_order_seq_cst) == 2);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r == 2 || r == 6);
  }
};

struct Exp19Spec {
  using method_t = std::function<ValueWrapper(Exp19Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp19Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp19Test, Exp19Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp19Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp19Test, B),
                  }});
