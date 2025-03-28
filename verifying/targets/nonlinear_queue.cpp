/**
 * ./build/verifying/targets/nonlinear_queue --tasks 4 --rounds 100000
 * --strategy rr --switches 1
 */
#include <atomic>
#include <cstring>

#include "../specs/queue.h"

const int N = 100;

// Implementation.
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

  void Reset() {
    head.store(0);
    for (int i = 0; i < N; ++i) a[i].store(0);
  }

  std::mutex m_;

  std::atomic<int> a[N];
  std::atomic<int> head{};
};

// Arguments generator.
auto generateInt(size_t unused_param) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

// Specify target structure and it's sequential specification.
using spec_t =
    ltest::Spec<Queue, spec::Queue, spec::QueueHash, spec::QueueEquals>;

LTEST_ENTRYPOINT(spec_t);

target_method(generateInt, void, Queue, Push, int);

target_method(ltest::generators::genEmpty, int, Queue, Pop);