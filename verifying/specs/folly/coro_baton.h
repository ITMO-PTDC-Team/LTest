//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_BATON_H
#define LTEST_FOLLY_CORO_BATON_H

#pragma once

#include <deque>
#include <optional>
#include <string>
#include <unordered_set>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

// Mixed sequential spec for folly::coro::Baton:
// - wait()  : dual
// - post()  : ordinary, wakes all current waiters
// - reset() : ordinary, clears only the posted flag
// - ready() : ordinary
struct FollyCoroBaton {
  bool posted = false;
  std::deque<int> waiting;
  std::unordered_set<int> ready_waits;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;
    ltest::ReserveRule r;
    r.wait_method = "wait";
    r.progress_methods = {"post"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));
    return p;
  }

  ValueWrapper Post(void* /*args*/) {
    posted = true;
    while (!waiting.empty()) {
      ready_waits.insert(waiting.front());
      waiting.pop_front();
    }
    return void_v;
  }

  ValueWrapper Reset(void* /*args*/) {
    posted = false;
    return void_v;
  }

  ValueWrapper Ready(void* /*args*/) {
    return ValueWrapper(posted);
  }

  void RequestWait(int op_id) {
    if (posted) {
      ready_waits.insert(op_id);
    } else {
      waiting.push_back(op_id);
    }
  }

  std::optional<ValueWrapper> FollowUpWait(int op_id) {
    if (!ready_waits.contains(op_id)) {
      return std::nullopt;
    }
    ready_waits.erase(op_id);
    return void_v;
  }

  static auto GetDualMethods() {
    using S = FollyCoroBaton;
    DualMethodMap<S> m;

    m.emplace("post", DualNonBlockingMethod<S>{
                          [](S* s, void* args) { return s->Post(args); }});
    m.emplace("reset", DualNonBlockingMethod<S>{
                           [](S* s, void* args) { return s->Reset(args); }});
    m.emplace("ready", DualNonBlockingMethod<S>{
                           [](S* s, void* args) { return s->Ready(args); }});

    DualRequestMethod<S> wait_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestWait(op_id); };
    DualFollowUpMethod<S> wait_fol =
        [](S* s, void* /*args*/, int op_id) { return s->FollowUpWait(op_id); };
    m.emplace("wait", DualBlockingMethod<S>{wait_req, wait_fol});

    return m;
  }
};

struct FollyCoroBatonHash {
  size_t operator()(const FollyCoroBaton& s) const {
    size_t h = static_cast<size_t>(s.posted) * 1315423911u;
    for (int op_id : s.waiting) {
      h ^= static_cast<size_t>(op_id + 1) * 2654435761u;
      h = (h << 6) ^ (h >> 2) ^ 0x9e3779b97f4a7c15ull;
    }
    for (int op_id : s.ready_waits) {
      h ^= static_cast<size_t>(op_id + 1) * 97531u;
    }
    return h;
  }
};

struct FollyCoroBatonEquals {
  bool operator()(const FollyCoroBaton& a,
                  const FollyCoroBaton& b) const {
    return a.posted == b.posted &&
           a.waiting == b.waiting &&
           a.ready_waits == b.ready_waits;
  }
};

}  // namespace spec

#endif  // LTEST_FOLLY_CORO_BATON_H
