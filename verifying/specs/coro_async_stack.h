//
// Created by d84370027 on 2/8/2026.
//

#ifndef CORO_ASYNC_STACK_H
#define CORO_ASYNC_STACK_H

#pragma once

#include <map>
#include <optional>
#include <tuple>
#include <unordered_map>
#include <vector>

#include "../../runtime/include/lincheck_dual.h"
#include "../../runtime/include/value_wrapper.h"
#include "../../runtime/include/workload_policy.h"

namespace spec {

// Dual sequential specification for coro::async_stack<T> (T = int).
//
// Model:
//
// - push(v) is a normal (nonblocking) method.
// - pop() is a dual method split into:
//     RequestPop(op_id): register pop reservation or take a value immediately
//     FollowUpPop(op_id): completes when the pop has been matched and has a
//     value
//
// Matching semantics:
// - If there is a pending pop reservation, push(v) satisfies the newest one
//   (LIFO), because your implementation stores pop operations in the stack head
//   and push matches against the head.
//
struct CoroAsyncStack {
  // Real values in the stack (LIFO: back() is top).
  std::vector<int> values;

  // Pending pop reservations (LIFO).
  std::vector<int> waiting_pops;  // op_id

  // Completed pop results waiting for follow-up completion: op_id -> value.
  std::unordered_map<int, int> ready_pop_value;

  // ----- Nonblocking: push(int) -----
  ValueWrapper Push(void* args) {
    auto real_args = reinterpret_cast<std::tuple<int>*>(args);
    int v = std::get<0>(*real_args);

    // If some pop is waiting, satisfy it (LIFO).
    if (!waiting_pops.empty()) {
      int pop_id = waiting_pops.back();
      waiting_pops.pop_back();
      ready_pop_value[pop_id] = v;
    } else {
      values.push_back(v);
    }
    return void_v;
  }

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;
    ltest::ReserveRule r;
    r.wait_method = "pop";
    r.progress_methods = {"push"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));
    return p;
  }

  // ----- Dual: pop() request phase -----
  void RequestPop(int op_id) {
    if (!values.empty()) {
      int v = values.back();
      values.pop_back();
      ready_pop_value[op_id] = v;
    } else {
      waiting_pops.push_back(op_id);
    }
  }

  // ----- Dual: pop() follow-up phase -----
  std::optional<ValueWrapper> FollowUpPop(int op_id) {
    auto it = ready_pop_value.find(op_id);
    if (it == ready_pop_value.end()) return std::nullopt;
    int v = it->second;
    ready_pop_value.erase(it);
    return ValueWrapper(v);
  }

  // Methods table for Dual checker.
  static auto GetDualMethods() {
    using S = CoroAsyncStack;
    DualMethodMap<S> m;

    // push(int): ordinary Invoke/Response operation
    DualNonBlockingMethod<S> push = [](S* s, void* args) -> ValueWrapper {
      return s->Push(args);
    };

    // pop(): dual (request+followup)
    DualRequestMethod<S> pop_req = [](S* s, void* /*args*/, int op_id) {
      s->RequestPop(op_id);
    };

    DualFollowUpMethod<S> pop_fol =
        [](S* s, void* /*args*/, int op_id) -> std::optional<ValueWrapper> {
      return s->FollowUpPop(op_id);
    };

    m.emplace("push", push);
    m.emplace("pop", DualBlockingMethod<S>{pop_req, pop_fol});
    return m;
  }
};

// Hash/equals: not required by recursive dual checker, but used by SpecDual
// type.
struct CoroAsyncStackHash {
  size_t operator()(const CoroAsyncStack& s) const {
    size_t h = 0;
    h ^= s.values.size() * 1315423911u;
    h ^= s.waiting_pops.size() * 2654435761u;
    h ^= s.ready_pop_value.size() * 97531u;
    // Note: content hashing is omitted (ok for now; collisions only affect
    // perf).
    return h;
  }
};

struct CoroAsyncStackEquals {
  bool operator()(const CoroAsyncStack& a, const CoroAsyncStack& b) const {
    return a.values == b.values && a.waiting_pops == b.waiting_pops &&
           a.ready_pop_value == b.ready_pop_value;
  }
};

}  // namespace spec

#endif  // CORO_ASYNC_STACK_H
