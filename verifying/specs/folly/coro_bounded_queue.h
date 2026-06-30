//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_BOUNDED_QUEUE_H
#define LTEST_FOLLY_CORO_BOUNDED_QUEUE_H

#pragma once

#include <algorithm>
#include <deque>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_set>
#include <unordered_map>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

// Sequential spec for folly::coro::BoundedQueue<int> with capacity 2.
//
// enqueue() and dequeue() are dual. Folly's enqueue coroutine can publish the
// value before the awaiting coroutine is resumed, so the request phase is what
// mutates the queue state. The follow-up only confirms that the caller resumed.
// try_dequeue() returns 0 when the queue is empty because target values are
// generated as positive integers.
struct FollyCoroBoundedQueue {
  static constexpr size_t kCapacity = 2;
  static constexpr int kEmptyTryDequeue = 0;

  struct WaitingEnqueue {
    int op_id;
    int value;

    bool operator==(const WaitingEnqueue&) const = default;
  };

  std::deque<int> elems;
  std::deque<WaitingEnqueue> waiting_enqueues;
  std::deque<int> waiting_dequeues;
  std::unordered_set<int> completed_enqueues;
  std::unordered_map<int, int> ready_dequeues;
  size_t available_capacity{kCapacity};

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;

    ltest::ReserveRule dequeue_wait;
    dequeue_wait.wait_method = "dequeue";
    dequeue_wait.progress_methods = {"enqueue", "try_enqueue"};
    dequeue_wait.reserve_threads = 1;
    p.reserve.push_back(std::move(dequeue_wait));

    ltest::ReserveRule enqueue_wait;
    enqueue_wait.wait_method = "enqueue";
    enqueue_wait.progress_methods = {"dequeue", "try_dequeue"};
    enqueue_wait.reserve_threads = 1;
    p.reserve.push_back(std::move(enqueue_wait));

    return p;
  }

  ValueWrapper TryEnqueue(void* args) {
    auto* tup = reinterpret_cast<std::tuple<int>*>(args);
    int v = std::get<0>(*tup);

    if (available_capacity == 0) {
      return ValueWrapper(false);
    }

    --available_capacity;
    MakeValueVisible(v);
    PromoteWaitingEnqueues();
    return ValueWrapper(true);
  }

  ValueWrapper TryDequeue(void* /*args*/) {
    if (elems.empty()) {
      return ValueWrapper(kEmptyTryDequeue);
    }

    int v = elems.front();
    elems.pop_front();
    ++available_capacity;
    PromoteWaitingEnqueues();
    return ValueWrapper(v);
  }

  void RequestEnqueue(int op_id, int value) {
    if (available_capacity == 0) {
      waiting_enqueues.push_back(WaitingEnqueue{op_id, value});
      return;
    }

    --available_capacity;
    completed_enqueues.insert(op_id);
    MakeValueVisible(value);
    PromoteWaitingEnqueues();
  }

  std::optional<ValueWrapper> FollowUpEnqueue(int op_id) {
    auto it = completed_enqueues.find(op_id);
    if (it == completed_enqueues.end()) {
      return std::nullopt;
    }

    completed_enqueues.erase(it);
    return void_v;
  }

  void RequestDequeue(int op_id) {
    if (!elems.empty()) {
      int v = elems.front();
      elems.pop_front();
      ready_dequeues[op_id] = v;
      ++available_capacity;
      PromoteWaitingEnqueues();
    } else {
      waiting_dequeues.push_back(op_id);
    }
  }

  std::optional<ValueWrapper> FollowUpDequeue(int op_id) {
    auto it = ready_dequeues.find(op_id);
    if (it == ready_dequeues.end()) {
      return std::nullopt;
    }

    int v = it->second;
    ready_dequeues.erase(it);
    return ValueWrapper(v);
  }

  static auto GetDualMethods() {
    using S = FollyCoroBoundedQueue;
    DualMethodMap<S> m;

    m.emplace("try_enqueue",
              DualNonBlockingMethod<S>{
                  [](S* s, void* args) { return s->TryEnqueue(args); }});
    m.emplace("try_dequeue",
              DualNonBlockingMethod<S>{
                  [](S* s, void* args) { return s->TryDequeue(args); }});

    DualRequestMethod<S> enqueue_req =
        [](S* s, void* args, int op_id) {
          auto* tup = reinterpret_cast<std::tuple<int>*>(args);
          s->RequestEnqueue(op_id, std::get<0>(*tup));
        };
    DualFollowUpMethod<S> enqueue_fol =
        [](S* s, void* /*args*/, int op_id) {
          return s->FollowUpEnqueue(op_id);
        };
    m.emplace("enqueue", DualBlockingMethod<S>{enqueue_req, enqueue_fol});

    DualRequestMethod<S> dequeue_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestDequeue(op_id); };
    DualFollowUpMethod<S> dequeue_fol =
        [](S* s, void* /*args*/, int op_id) {
          return s->FollowUpDequeue(op_id);
        };
    m.emplace("dequeue", DualBlockingMethod<S>{dequeue_req, dequeue_fol});

    return m;
  }

 private:
  void MakeValueVisible(int value) {
    if (!waiting_dequeues.empty()) {
      int op_id = waiting_dequeues.front();
      waiting_dequeues.pop_front();
      ready_dequeues[op_id] = value;
    } else {
      elems.push_back(value);
    }
  }

  void PromoteWaitingEnqueues() {
    while (!waiting_enqueues.empty() && available_capacity > 0) {
      WaitingEnqueue w = waiting_enqueues.front();
      waiting_enqueues.pop_front();
      --available_capacity;
      completed_enqueues.insert(w.op_id);
      MakeValueVisible(w.value);
    }
  }
};

struct FollyCoroBoundedQueueHash {
  size_t operator()(const FollyCoroBoundedQueue& s) const {
    size_t h = 0;
    for (int v : s.elems) {
      h ^= static_cast<size_t>(v + 17) * 2654435761u;
      h = (h << 6) ^ (h >> 2) ^ 0x9e3779b97f4a7c15ull;
    }
    for (const auto& w : s.waiting_enqueues) {
      h ^= static_cast<size_t>(w.op_id + 1) * 97531u;
      h ^= static_cast<size_t>(w.value + 17) * 424242u;
    }
    for (int op_id : s.completed_enqueues) {
      h ^= static_cast<size_t>(op_id + 1) * 65537u;
    }
    for (int op_id : s.waiting_dequeues) {
      h ^= static_cast<size_t>(op_id + 1) * 1315423911u;
    }
    for (const auto& [op_id, v] : s.ready_dequeues) {
      h ^= static_cast<size_t>(op_id + 1) * 2166136261u;
      h ^= static_cast<size_t>(v + 17) * 16777619u;
    }
    h ^= s.available_capacity * 1099511628211ull;
    return h;
  }
};

struct FollyCoroBoundedQueueEquals {
  bool operator()(const FollyCoroBoundedQueue& a,
                  const FollyCoroBoundedQueue& b) const {
    return a.elems == b.elems &&
           a.waiting_enqueues == b.waiting_enqueues &&
           a.completed_enqueues == b.completed_enqueues &&
           a.waiting_dequeues == b.waiting_dequeues &&
           a.ready_dequeues == b.ready_dequeues &&
           a.available_capacity == b.available_capacity;
  }
};

}  // namespace spec

#endif  // LTEST_FOLLY_CORO_BOUNDED_QUEUE_H
