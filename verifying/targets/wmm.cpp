#include <atomic>
#include <iostream>
#include <sstream>
#include <tuple>

#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"
#include "verifying/specs/register.h"

struct WmmTest {
  WmmTest() {}
  WmmTest(const WmmTest&) {}
  WmmTest& operator=(const WmmTest&) {
    return *this;
  }

  std::atomic<int> x{0}, y{0};
  int r1 = -1, r2 = -1;

  // Example 1
  non_atomic void Exp1_A() {
    r1 = y.load(std::memory_order_seq_cst);
    x.store(1, std::memory_order_seq_cst);
    std::string out = "r1 = " + std::to_string(r1) + "\n";
    std::cout << out;

    assert(!(r1 == 1 && r2 == 1));
  }

  non_atomic void Exp1_B() {
    r2 = x.load(std::memory_order_seq_cst);
    y.store(1, std::memory_order_seq_cst);
    std::string out = "r2 = " + std::to_string(r2) + "\n";
    std::cout << out;

    assert(!(r1 == 1 && r2 == 1));
  }

  // Example 2
  non_atomic void Exp2_A() {
    y.store(1, std::memory_order_relaxed);
    x.store(2, std::memory_order_seq_cst);
  }

  non_atomic void Exp2_B() {
    if (x.load(std::memory_order_seq_cst) == 2) {
      r1 = y.load(std::memory_order_relaxed);
      assert(r1 == 1);
    }
    std::string out = "r1 = " + std::to_string(r1) + "\n";
    std::cout << out;
  }

  // Example 3
  non_atomic void Exp3_A() {
    y.store(20, std::memory_order_seq_cst);
    x.store(10, std::memory_order_seq_cst);
  }

  non_atomic void Exp3_B() {
    if (x.load(std::memory_order_seq_cst) == 10) {
      assert(y.load(std::memory_order_seq_cst) == 20);
      y.store(10, std::memory_order_seq_cst);
    }
  }

  non_atomic void Exp3_C() {
    if (y.load(std::memory_order_seq_cst) == 10) {
      assert(x.load(std::memory_order_seq_cst) == 10);
    }
  }

  // Example 4
  non_atomic void Exp4_A() {
    y.store(20, std::memory_order_relaxed);
    x.store(10, std::memory_order_relaxed);
  }

  non_atomic void Exp4_B() {
    if (x.load(std::memory_order_relaxed) == 10) {
      assert(y.load(std::memory_order_relaxed) == 20); // could fail
      y.store(10, std::memory_order_relaxed);
    }
  }

  non_atomic void Exp4_C() {
    if (y.load(std::memory_order_relaxed) == 10) {
      assert(x.load(std::memory_order_relaxed) == 10); // could fail
    }
  }

  // Example 5
  non_atomic void Exp5_A() {
    y.store(20, std::memory_order_release);
    x.store(10, std::memory_order_release);
  }

  non_atomic void Exp5_B() {
    if (x.load(std::memory_order_acquire) == 10) {
      assert(y.load(std::memory_order_acquire) == 20);
      y.store(10, std::memory_order_release);
    }
  }

  non_atomic void Exp5_C() {
    if (y.load(std::memory_order_acquire) == 10) {
      assert(x.load(std::memory_order_acquire) == 10);
    }
  }

  // Example 6 (TODO: fix mixed memory order accesses, see https://gcc.gnu.org/wiki/Atomic/GCCMM/AtomicSync)
  // Note: Requires further investigation, because it seems that sc support is not sound right now.
  //       Fix it.
  non_atomic void Exp6_A() {
    y.store(20, std::memory_order_relaxed);
    x.store(10, std::memory_order_seq_cst);
  }

  non_atomic void Exp6_B() {
    if (x.load(std::memory_order_relaxed) == 10) {
      assert(y.load(std::memory_order_seq_cst) == 20);
      y.store(10, std::memory_order_relaxed);
    }
  }

  non_atomic void Exp6_C() {
    if (y.load(std::memory_order_acquire) == 10) {
      assert(x.load(std::memory_order_acquire) == 10);
    }
  }

  // Example 7
  non_atomic void Exp7_A() {
    int expected = 0;
    do {
      expected = x.load(std::memory_order_seq_cst);
    } while (!x.compare_exchange_weak(expected, expected + 1, std::memory_order_seq_cst));
    assert(expected == 0);
    assert(x.load() == 1);
  }

  non_atomic void Exp7_B() {
    int r = x.load(std::memory_order_seq_cst);
    std::cout << "r = " << r << "\n";
    assert(r >= 0 && r <= 1);
  }

  // Example 8
  non_atomic void Exp8_A() {
    int expected;
    do {
      expected = x.load(std::memory_order_seq_cst);
    } while (!x.compare_exchange_weak(expected, expected + 1, std::memory_order_seq_cst));
    int r = x.load(std::memory_order_seq_cst);
    assert(expected == 0 || expected == 1);
    assert(r >= 1 && r <= 2);
  }

  non_atomic void Exp8_B() {
    x.store(1, std::memory_order_seq_cst);
    int r = x.load(std::memory_order_seq_cst);
    std::cout << "r = " << r << "\n";
    assert(r >= 1 && r <= 2);
  }

  // Example 9 (weak rmw)
  non_atomic void Exp9_A() {
    int expected = 0;
    while (!x.compare_exchange_strong(expected, 1, std::memory_order_relaxed, std::memory_order_relaxed)) {
      expected = 0;
    }
    y.store(1, std::memory_order_relaxed);
  }

  non_atomic void Exp9_B() {
    int a = y.load(std::memory_order_relaxed);
    int b = x.load(std::memory_order_relaxed);
    std::stringstream ss;
    ss << "a = " << a << ", b = " << b << "\n";
    std::cout << ss.str();
    assert(!(a == 1 && b == 0)); // Could fail
  }
};

struct LinearWmmSpec {
  using method_t = std::function<ValueWrapper(LinearWmmSpec *l, void *)>;

  static auto GetMethods() {
    method_t func = [](LinearWmmSpec *l, void *) -> ValueWrapper { return void_v; };

    return std::map<std::string, method_t>{
      {"Exp1_A", func},
      {"Exp1_B", func},

      {"Exp2_A", func},
      {"Exp2_B", func},

      {"Exp3_A", func},
      {"Exp3_B", func},
      {"Exp3_C", func},

      {"Exp4_A", func},
      {"Exp4_B", func},
      {"Exp4_C", func},

      {"Exp5_A", func},
      {"Exp5_B", func},
      {"Exp5_C", func},

      {"Exp6_A", func},
      {"Exp6_B", func},
      {"Exp6_C", func},

      {"Exp7_A", func},
      {"Exp7_B", func},

      {"Exp8_A", func},
      {"Exp8_B", func},

      {"Exp9_A", func},
      {"Exp9_B", func},
    };
  }
};

struct LinearWmmHash {
  size_t operator()(const LinearWmmSpec &r) const { return 1; }
};

struct LinearWmmEquals {
  bool operator()(const LinearWmmSpec &lhs, const LinearWmmSpec &rhs) const {
    return true;
  }
};

using spec_t =
    ltest::Spec<WmmTest, LinearWmmSpec, LinearWmmHash, LinearWmmEquals>;

// Uncomment required tests when needed
LTEST_ENTRYPOINT(spec_t, 
  {
    {
      method_invocation(std::tuple(), void, WmmTest, Exp1_A)
    },
    {
      method_invocation(std::tuple(), void, WmmTest, Exp1_B)
    }
  },
  // {
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp2_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp2_B)
  //   }
  // },
  // {
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp3_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp3_B)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp3_C)
  //   }
  // },
  // { // could fail
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp4_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp4_B)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp4_C)
  //   }
  // },
  // {
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp5_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp5_B)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp5_C)
  //   }
  // },
  // { // TODO: fails but should not, probably due to missing support for release sequences
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp6_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp6_B)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp6_C)
  //   }
  // },
  // {
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp7_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp7_B)
  //   }
  // },
  // {
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp8_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp8_B)
  //   }
  // },
  // {
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp9_A)
  //   },
  //   {
  //     method_invocation(std::tuple(), void, WmmTest, Exp9_B)
  //   }
  // },
);