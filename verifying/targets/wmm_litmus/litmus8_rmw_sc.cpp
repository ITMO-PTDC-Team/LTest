#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

struct Exp8Test {
  std::atomic<int> x{0};

  non_atomic void A() {
    int expected;
    do {
      expected = x.load(std::memory_order_seq_cst);
    } while (!x.compare_exchange_weak(expected, expected + 1,
                                      std::memory_order_seq_cst));
    int r = x.load(std::memory_order_seq_cst);
    rassert(expected == 0 || expected == 1);
    rassert(r >= 1 && r <= 2);
  }

  non_atomic void B() {
    x.store(1, std::memory_order_seq_cst);
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 1 && r <= 2);
  }
};

struct Exp8Spec {
  using method_t = std::function<ValueWrapper(Exp8Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp8Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t = ltest::Spec<Exp8Test, Exp8Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp8Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp8Test, B),
                  }});
