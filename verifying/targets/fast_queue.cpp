/**
 * ./build/verifying/targets/fast_queue --strategy tla --tasks 4 --rounds 50000 --switches 1
 */
#include <atomic>
#include <iostream>
#include <vector>

#include "../specs/bounded_queue.h"

template <typename T>
struct Node {
  std::atomic<size_t> generation;
  T val;

  Node() : generation{} {}

  Node(const std::atomic<T> &a) : generation(a.load()) {}

  Node(const Node &other) : generation(other.generation.load()) {}

  Node &operator=(const Node &other) {
    generation.store(other.generation.load());
    return *this;
  }
};

constexpr int size = 2;

auto GenerateInt(size_t thread_num) {
  return ltest::generators::MakeSingleArg(rand() % 10 + 1);
}

class MPMCBoundedQueue {
 public:
  explicit MPMCBoundedQueue() : max_size{size - 1} {
    vec.resize(size);
    for (size_t i = 0; i < size; ++i) {
      vec[i].generation.store(i, std::memory_order_relaxed);
    }
  }

  void Reset() {
    for (size_t i = 0; i < size; ++i) {
      vec[i].generation.store(i);
    }
    head.store(0);
    tail.store(0);
  }

  NON_ATOMIC int Push(int value) {
    while (true) {
      auto h = head.load(/*std::memory_order_relaxed*/);
      auto hid = h & max_size;
      auto gen = vec[hid].generation.load(/*std::memory_order_relaxed*/);
      if (gen == h) {
        if (head.compare_exchange_weak(h,
                                       h + 1 /*, std::memory_order_acquire*/)) {
          // I am owner of the element.
          vec[hid].val = value;
          vec[hid].generation.fetch_add(1 /*, std::memory_order_release*/);
          return true;
        }
      } else if (gen < h) {
        return false;
      }
    }
  }

  NON_ATOMIC int Pop() {
    while (true) {
      auto t = tail.load(/*std::memory_order_relaxed*/);
      auto tid = t & max_size;
      auto gen = vec[tid].generation.load(/*std::memory_order_relaxed*/);
      if (gen == t + 1) {
        if (tail.compare_exchange_weak(
                t, t + 1 /*, std::memory_order_acquire*/)) {
          int ret = std::move(vec[tid].val);
          vec[tid].generation.fetch_add(
              max_size /*, std::memory_order_release*/);
          return ret;
        }
      } else {
        if (gen < t + 1) {
          return 0;
        }
      }
    }
  }

 private:
  size_t max_size;
  std::vector<Node<int>> vec;

  std::atomic<size_t> head{};
  std::atomic<size_t> tail{};
};

// 0 1 2 3 4 5 6 7
// h = 0
// PUSH 5
// 5
// 7 1 2 3 4 5 6 7
// POP
// 1 == tail + 1? 1 == 1

using SpecT = ltest::Spec<MPMCBoundedQueue, spec::Queue, spec::QueueHash,
                          spec::QueueEquals>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(GenerateInt, int, MPMCBoundedQueue, Push, int);

TARGET_METHOD(ltest::generators::GenEmpty, int, MPMCBoundedQueue, Pop);