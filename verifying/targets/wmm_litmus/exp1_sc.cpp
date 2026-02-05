#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp1Test {
  std::atomic<int> x{0}, y{0};
  int r1 = -1, r2 = -1;

  non_atomic void A() {
    r1 = y.load(std::memory_order_seq_cst);
    x.store(1, std::memory_order_seq_cst);
    assert(!(r1 == 1 && r2 == 1));
  }

  non_atomic void B() {
    r2 = x.load(std::memory_order_seq_cst);
    y.store(1, std::memory_order_seq_cst);
    assert(!(r1 == 1 && r2 == 1));
  }
};

struct Exp1Spec {
  using method_t = std::function<ValueWrapper(Exp1Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp1Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t = ltest::Spec<Exp1Test, Exp1Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp1Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp1Test, B),
                  }});
