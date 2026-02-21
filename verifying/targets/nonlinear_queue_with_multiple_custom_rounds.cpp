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

LTEST_ENTRYPOINT(spec_t,
                 // custom round 1
                 {{
                      method_invocation(std::tuple(1), void, Queue, Push, int),
                      method_invocation(std::tuple(2), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(3), void, Queue, Push, int),
                      method_invocation(std::tuple(4), void, Queue, Push, int),
                  }},
                 // custom round 2
                 {{
                      method_invocation(std::tuple(1), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(2), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(3), void, Queue, Push, int),
                  },
                  {
                      method_invocation(std::tuple(4), void, Queue, Push, int),
                  }},
                 // custom round 3
                 {
                     {method_invocation(std::tuple(1), void, Queue, Push, int),
                      method_invocation(std::tuple(), int, Queue, Pop)},
                 });

// Then some generated rounds will be executed after all custom rounds.

// Arguments generator.
auto generateInt(size_t unused_param) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

// Targets.
target_method(generateInt, void, Queue, Push, int);
target_method(ltest::generators::genEmpty, int, Queue, Pop);