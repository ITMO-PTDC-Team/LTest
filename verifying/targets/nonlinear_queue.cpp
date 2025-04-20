/**
 * ./build/verifying/targets/nonlinear_queue --tasks 4 --rounds 100000
 * --strategy rr --switches 1
 */
#include <atomic>
#include <cstring>

#include "../specs/queue.h"

constexpr int n = 100;

// Implementation.
struct Queue {
  Queue() {}

  NON_ATOMIC void Push(int v) {
    int pos = head.fetch_add(1);
    a[pos] = v;
  }

  NON_ATOMIC int Pop() {
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
    for (int i = 0; i < n; ++i) a[i].store(0);
  }

  std::array<std::atomic<int>, n> a;
  std::atomic<int> head{};
};

// Arguments generator.
auto GenerateInt(size_t unused_param) {
  return ltest::generators::MakeSingleArg(rand() % 10 + 1);
}

// Specify target structure and it's sequential specification.
using SpecT =
    ltest::Spec<Queue, spec::Queue<>, spec::QueueHash<>, spec::QueueEquals<>>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(GenerateInt, void, Queue, Push, int);

TARGET_METHOD(ltest::generators::GenEmpty, int, Queue, Pop);