//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_UNBOUNDED_QUEUE_H
#define LTEST_FOLLY_CORO_UNBOUNDED_QUEUE_H

#pragma once

#include <deque>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

// Sequential spec for folly::coro::UnboundedQueue<int>.
//
// enqueue() is total and synchronous in Folly. dequeue() is dual and waits
// while the queue is empty. Target values are generated as positive integers,
// so try_dequeue() returns 0 to represent an empty queue.
struct FollyCoroUnboundedQueue {
  static constexpr int kEmptyTryDequeue = 0;

  std::deque<int> elems;
  std::deque<int> waiting_dequeues;
  std::unordered_map<int, int> ready_dequeues;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;
    ltest::ReserveRule r;
    r.wait_method = "dequeue";
    r.progress_methods = {"enqueue"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));

    return p;
  }

  ValueWrapper Enqueue(void* args) {
    auto* tup = reinterpret_cast<std::tuple<int>*>(args);
    int v = std::get<0>(*tup);

    if (!waiting_dequeues.empty()) {
      int op_id = waiting_dequeues.front();
      waiting_dequeues.pop_front();
      ready_dequeues[op_id] = v;
    } else {
      elems.push_back(v);
    }

    return void_v;
  }

  ValueWrapper TryDequeue(void* /*args*/) {
    if (elems.empty()) {
      return ValueWrapper(kEmptyTryDequeue);
    }

    int v = elems.front();
    elems.pop_front();
    return ValueWrapper(v);
  }

  void RequestDequeue(int op_id) {
    if (!elems.empty()) {
      int v = elems.front();
      elems.pop_front();
      ready_dequeues[op_id] = v;
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
    using S = FollyCoroUnboundedQueue;
    DualMethodMap<S> m;

    m.emplace("enqueue", DualNonBlockingMethod<S>{
                             [](S* s, void* args) { return s->Enqueue(args); }});
    m.emplace("try_dequeue",
              DualNonBlockingMethod<S>{
                  [](S* s, void* args) { return s->TryDequeue(args); }});

    DualRequestMethod<S> dequeue_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestDequeue(op_id); };
    DualFollowUpMethod<S> dequeue_fol =
        [](S* s, void* /*args*/, int op_id) {
          return s->FollowUpDequeue(op_id);
        };
    m.emplace("dequeue", DualBlockingMethod<S>{dequeue_req, dequeue_fol});

    return m;
  }
};

struct FollyCoroUnboundedQueueHash {
  size_t operator()(const FollyCoroUnboundedQueue& s) const {
    size_t h = 0;
    for (int v : s.elems) {
      h ^= static_cast<size_t>(v + 17) * 2654435761u;
      h = (h << 6) ^ (h >> 2) ^ 0x9e3779b97f4a7c15ull;
    }
    for (int op_id : s.waiting_dequeues) {
      h ^= static_cast<size_t>(op_id + 1) * 97531u;
    }
    for (const auto& [op_id, v] : s.ready_dequeues) {
      h ^= static_cast<size_t>(op_id + 1) * 1315423911u;
      h ^= static_cast<size_t>(v + 17) * 424242u;
    }
    return h;
  }
};

struct FollyCoroUnboundedQueueEquals {
  bool operator()(const FollyCoroUnboundedQueue& a,
                  const FollyCoroUnboundedQueue& b) const {
    return a.elems == b.elems &&
           a.waiting_dequeues == b.waiting_dequeues &&
           a.ready_dequeues == b.ready_dequeues;
  }
};

}  // namespace spec

#endif  // LTEST_FOLLY_CORO_UNBOUNDED_QUEUE_H
