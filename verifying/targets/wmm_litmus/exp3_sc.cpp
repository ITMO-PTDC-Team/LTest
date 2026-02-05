#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp3Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(20, std::memory_order_seq_cst);
    x.store(10, std::memory_order_seq_cst);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_seq_cst) == 10) {
      assert(y.load(std::memory_order_seq_cst) == 20);
      y.store(10, std::memory_order_seq_cst);
    }
  }

  non_atomic void C() {
    if (y.load(std::memory_order_seq_cst) == 10) {
      assert(x.load(std::memory_order_seq_cst) == 10);
    }
  }
};

struct Exp3Spec {
  using method_t = std::function<ValueWrapper(Exp3Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp3Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
        {"C", func},
    };
  }
};

using spec_t = ltest::Spec<Exp3Test, Exp3Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp3Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp3Test, B),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp3Test, C),
                  }});
