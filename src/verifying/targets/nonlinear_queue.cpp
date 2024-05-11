/**
 * ./verify.py build --src ./targets/nonlinear_queue.cpp
 * ./verify.py run -v  --tasks 4 --rounds 100000 --strategy tla --switches 1
 */
#include <atomic>
#include <cstring>
#include <iostream>

#include "../specs/queue.h"

const int N = 100;

// Implementation.
struct Queue {
  Queue() {}

  void Push(int v);
  int Pop();
  void Reset() {
    head.store(0);
    for (int i = 0; i < N; ++i) a[i].store(0);
  }

  std::atomic<int> a[N];
  std::atomic<int> head{};
};

// Arguments generator.
auto generateInt() { return ltest::generators::makeSingleArg(rand() % 10 + 1); }

// Targets.
target_method(generateInt, void, Queue, Push, int v) {
  int pos = head.fetch_add(1);
  a[pos] = v;
}

target_method(ltest::generators::genEmpty, int, Queue, Pop) {
  int last = head.load();
  for (int i = 0; i < last; ++i) {
    int e = a[i].load();
    if (e != 0 && a[i].compare_exchange_strong(e, 0)) {
      return e;
    }
  }
  return 0;
}

// Specify target structure and it's sequential specification.
using spec_t =
    ltest::Spec<Queue, spec::Queue<>, spec::QueueHash<>, spec::QueueEquals<>>;

LTEST_ENTRYPOINT(spec_t);
