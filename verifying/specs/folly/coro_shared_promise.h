//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_SHARED_PROMISE_H
#define LTEST_FOLLY_CORO_SHARED_PROMISE_H

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

// Sequential spec for folly::coro::SharedPromise<int>.
//
// get() is dual: every caller gets the value once set_value() fulfills the
// promise. set_value() is exposed as try-set and returns false after the first
// successful fulfillment.
struct FollyCoroSharedPromise {
  std::optional<int> value;
  std::deque<int> waiting_gets;
  std::unordered_map<int, int> ready_gets;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;
    ltest::ReserveRule r;
    r.wait_method = "get";
    r.progress_methods = {"set_value"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));
    return p;
  }

  ValueWrapper SetValue(void* args) {
    auto* tup = reinterpret_cast<std::tuple<int>*>(args);
    int v = std::get<0>(*tup);

    if (value.has_value()) {
      return ValueWrapper(false);
    }

    value = v;
    while (!waiting_gets.empty()) {
      int op_id = waiting_gets.front();
      waiting_gets.pop_front();
      ready_gets[op_id] = v;
    }
    return ValueWrapper(true);
  }

  ValueWrapper IsFulfilled(void* /*args*/) const {
    return ValueWrapper(value.has_value());
  }

  void RequestGet(int op_id) {
    if (value.has_value()) {
      ready_gets[op_id] = *value;
    } else {
      waiting_gets.push_back(op_id);
    }
  }

  std::optional<ValueWrapper> FollowUpGet(int op_id) {
    auto it = ready_gets.find(op_id);
    if (it == ready_gets.end()) {
      return std::nullopt;
    }

    int v = it->second;
    ready_gets.erase(it);
    return ValueWrapper(v);
  }

  static auto GetDualMethods() {
    using S = FollyCoroSharedPromise;
    DualMethodMap<S> m;

    m.emplace("set_value",
              DualNonBlockingMethod<S>{
                  [](S* s, void* args) { return s->SetValue(args); }});
    m.emplace("is_fulfilled",
              DualNonBlockingMethod<S>{
                  [](S* s, void* args) { return s->IsFulfilled(args); }});

    DualRequestMethod<S> get_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestGet(op_id); };
    DualFollowUpMethod<S> get_fol =
        [](S* s, void* /*args*/, int op_id) { return s->FollowUpGet(op_id); };
    m.emplace("get", DualBlockingMethod<S>{get_req, get_fol});

    return m;
  }
};

struct FollyCoroSharedPromiseHash {
  size_t operator()(const FollyCoroSharedPromise& s) const {
    size_t h = s.value.has_value() ? static_cast<size_t>(*s.value + 17) : 0;
    for (int op_id : s.waiting_gets) {
      h ^= static_cast<size_t>(op_id + 1) * 97531u;
    }
    for (const auto& [op_id, v] : s.ready_gets) {
      h ^= static_cast<size_t>(op_id + 1) * 1315423911u;
      h ^= static_cast<size_t>(v + 17) * 424242u;
    }
    return h;
  }
};

struct FollyCoroSharedPromiseEquals {
  bool operator()(const FollyCoroSharedPromise& a,
                  const FollyCoroSharedPromise& b) const {
    return a.value == b.value &&
           a.waiting_gets == b.waiting_gets &&
           a.ready_gets == b.ready_gets;
  }
};

}  // namespace spec

#endif  // LTEST_FOLLY_CORO_SHARED_PROMISE_H
