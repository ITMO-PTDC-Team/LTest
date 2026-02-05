#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp2Test {
  std::atomic<int> x{0}, y{0};
  int r1 = -1;

  non_atomic void A() {
    y.store(1, std::memory_order_relaxed);
    x.store(2, std::memory_order_seq_cst);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_seq_cst) == 2) {
      r1 = y.load(std::memory_order_relaxed);
      assert(r1 == 1);
    }
  }
};

struct Exp2Spec {
  using method_t = std::function<ValueWrapper(Exp2Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp2Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t = ltest::Spec<Exp2Test, Exp2Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp2Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp2Test, B),
                  }});
