#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Prefix ++ on atomic<int> uses sequential consistency (standard default).
// A increments from 0 to 1 and checks the returned value. B only observes
// x in {0,1}. This should not fail if seq_cst RMW and loads are modeled soundly.

struct Exp13Test {
  std::atomic<int> x{0};

  non_atomic void A() {
    int v = ++x;
    rassert(v == 1);
    rassert(x.load(std::memory_order_seq_cst) == 1);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r >= 0 && r <= 1);
  }
};

struct Exp13Spec {
  using method_t = std::function<ValueWrapper(Exp13Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp13Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

using spec_t =
    ltest::Spec<Exp13Test, Exp13Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp13Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp13Test, B),
                  }});
