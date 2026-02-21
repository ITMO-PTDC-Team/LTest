/**
 * Nonlinear queue with a single large custom round that is non-linearizable.
 * The round has many groups and operations to stress concurrent Push/Pop
 * interleaving and expose the bug (Pop scanning from 0 can return wrong order).
 *
 * Tests: run with --strategy pct (exploration_runs 1000) or --strategy random
 * (exploration_runs 100000), both with --minimize.
 */
#include <atomic>
#include <cstring>

#include "../specs/queue.h"
#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"

const int N = 100;

// Implementation copy-pasted from `nonlinear_queue.cpp`
struct Queue {
  Queue() {}

  non_atomic void Push(int v) {
    int pos = head.fetch_add(1);
    a[pos] = v;
  }

  non_atomic int Pop() {
    int last = head.load();
    for (int i = 0; i < last; ++i) {
      int e = a[i].load();
      if (e != 0 && a[i].compare_exchange_strong(e, 0)) {
        return e;
      }
    }
    return 0;
  }

  std::atomic<int> a[N];
  std::atomic<int> head{};
};

// Specify target structure and it's sequential specification.
using spec_t =
    ltest::Spec<Queue, spec::Queue<>, spec::QueueHash, spec::QueueEquals>;

// Single large custom round: many groups with concurrent Push/Pop.
// Non-linearizable because Pop scans from index 0 and concurrent operations can
// produce wrong ordering.
LTEST_ENTRYPOINT(spec_t,
                 {{
                      method_invocation(std::tuple(1), void, Queue, Push, int),
                      method_invocation(std::tuple(2), void, Queue, Push, int),
                      method_invocation(std::tuple(3), void, Queue, Push, int),
                      method_invocation(std::tuple(4), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(5), void, Queue, Push, int),
                      method_invocation(std::tuple(6), void, Queue, Push, int),
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                  },
                  {
                      method_invocation(std::tuple(7), void, Queue, Push, int),
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                  },
                  {
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(8), void, Queue, Push, int),
                      method_invocation(std::tuple(9), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(10), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                  },
                  {
                      method_invocation(std::tuple(11), void, Queue, Push, int),
                      method_invocation(std::tuple(12), void, Queue, Push, int),
                      method_invocation(std::tuple(), int, Queue, Pop),
                  },
                  {
                      method_invocation(std::tuple(), int, Queue, Pop),
                      method_invocation(std::tuple(), int, Queue, Pop),
                  }});

// Arguments generator.
auto generateInt(size_t unused_param) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

// Targets.
target_method(generateInt, void, Queue, Push, int);
target_method(ltest::generators::genEmpty, int, Queue, Pop);
