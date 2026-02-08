#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// Example 6 (TODO: fix mixed memory order accesses, see
// https://gcc.gnu.org/wiki/Atomic/GCCMM/AtomicSync) Note: Requires further
// investigation, because it seems that sc support is not sound right now.
//       Fix it.
struct Exp6Test {
  std::atomic<int> x{0}, y{0};

  non_atomic void A() {
    y.store(20, std::memory_order_relaxed);
    x.store(10, std::memory_order_seq_cst);
  }

  non_atomic void B() {
    if (x.load(std::memory_order_relaxed) == 10) {
      rassert(y.load(std::memory_order_seq_cst) == 20);
      y.store(10, std::memory_order_relaxed);
    }
  }

  non_atomic void C() {
    if (y.load(std::memory_order_acquire) == 10) {
      rassert(x.load(std::memory_order_acquire) == 10);
    }
  }
};

struct Exp6Spec {
  using method_t = std::function<ValueWrapper(Exp6Spec *, void *)>;
  static auto GetMethods() {
    method_t func = [](Exp6Spec *, void *) -> ValueWrapper { return void_v; };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
        {"C", func},
    };
  }
};

using spec_t = ltest::Spec<Exp6Test, Exp6Spec, LinearWmmHash, LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp6Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp6Test, B),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp6Test, C),
                  }});
