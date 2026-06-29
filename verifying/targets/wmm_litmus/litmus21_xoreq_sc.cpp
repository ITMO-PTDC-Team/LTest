#include <atomic>
#include <cassert>
#include <map>
#include <string>

#include "verifying/targets/wmm_litmus/litmus_common.h"

// operator^= with seq_cst. x starts at 3; A XORs 1 to get 2. B observes x in
// {2,3}. Should not fail under seq_cst.

struct Exp21Test {
  std::atomic<int> x{3};

  non_atomic void A() {
    int v = (x ^= 1);
    rassert(v == 2);
    rassert(x.load(std::memory_order_seq_cst) == 2);
  }

  non_atomic void B() {
    int r = x.load(std::memory_order_seq_cst);
    rassert(r == 2 || r == 3);
  }
};

using spec_t = ltest::Spec<Exp21Test, LitmusTwoThreadsSpec, LinearWmmHash,
                           LinearWmmEquals>;

LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(), void, Exp21Test, A),
                  },
                  {
                      method_invocation(std::tuple(), void, Exp21Test, B),
                  }});
