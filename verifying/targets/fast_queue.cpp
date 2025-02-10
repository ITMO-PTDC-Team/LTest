/**
 * ./build/verifying/targets/fast_queue --strategy tla --tasks 4 --rounds 50000 --switches 1
 *
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

const int size = 2;

auto generateInt(size_t thread_num) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

class MPMCBoundedQueue {
 public:
  explicit MPMCBoundedQueue() : max_size_{size - 1} {
    vec_.resize(size);
    for (size_t i = 0; i < size; ++i) {
      vec_[i].generation.store(i, std::memory_order_relaxed);
    }
  }

  void Reset() {
    for (size_t i = 0; i < size; ++i) {
      vec_[i].generation.store(i);
    }
    head_.store(0);
    tail_.store(0);
  }

  non_atomic int Push(int value) {
    while (true) {
      auto h = head_.load(/*std::memory_order_relaxed*/);
      auto hid = h & max_size_;
      auto gen = vec_[hid].generation.load(/*std::memory_order_relaxed*/);
      if (gen == h) {
        if (head_.compare_exchange_weak(
                h, h + 1 /*, std::memory_order_acquire*/)) {
          // I am owner of the element.
          vec_[hid].val = value;
          vec_[hid].generation.fetch_add(1 /*, std::memory_order_release*/);
          return true;
        }
      } else if (gen < h) {
        return false;
      }
    }
  }

  non_atomic int Pop() {
    while (true) {
      auto t = tail_.load(/*std::memory_order_relaxed*/);
      auto tid = t & max_size_;
      auto gen = vec_[tid].generation.load(/*std::memory_order_relaxed*/);
      if (gen == t + 1) {
        if (tail_.compare_exchange_weak(
                t, t + 1 /*, std::memory_order_acquire*/)) {
          int ret = std::move(vec_[tid].val);
          vec_[tid].generation.fetch_add(
              max_size_ /*, std::memory_order_release*/);
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
  size_t max_size_;
  std::vector<Node<int>> vec_;

  std::atomic<size_t> head_{};
  std::atomic<size_t> tail_{};
};

// 0 1 2 3 4 5 6 7
// h = 0
// PUSH 5
// 5
// 7 1 2 3 4 5 6 7
// POP
// 1 == tail + 1? 1 == 1

target_method(generateInt, int, MPMCBoundedQueue, Push, int);

target_method(ltest::generators::genEmpty, int, MPMCBoundedQueue, Pop);

using spec_t = ltest::Spec<MPMCBoundedQueue, spec::Queue, spec::QueueHash,
                           spec::QueueEquals>;

LTEST_ENTRYPOINT(spec_t);
