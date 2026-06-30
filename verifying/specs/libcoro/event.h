#pragma once

#include <unordered_set>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

struct LibcoroEvent {
  bool is_set = false;
  std::unordered_set<int> waiting;
  std::unordered_set<int> ready;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;

    ltest::PrefixBudgetRule r;
    r.wait_method = "wait";
    r.progress_methods = {"set"};
    r.reserve_threads = 1;
    r.initial_credit = 0;
    p.prefix_budget.push_back(std::move(r));

    return p;
  }

  ValueWrapper Set(void* /*args*/) {
    is_set = true;
    for (int op_id : waiting) {
      ready.insert(op_id);
    }
    waiting.clear();
    return void_v;
  }

  ValueWrapper Reset(void* /*args*/) {
    is_set = false;
    return void_v;
  }

  ValueWrapper IsSet(void* /*args*/) {
    return ValueWrapper(is_set);
  }

  void RequestWait(int op_id) {
    if (is_set) {
      ready.insert(op_id);
    } else {
      waiting.insert(op_id);
    }
  }

  std::optional<ValueWrapper> FollowUpWait(int op_id) {
    if (!ready.contains(op_id)) {
      return std::nullopt;
    }
    ready.erase(op_id);
    return void_v;
  }

  static auto GetDualMethods() {
    using S = LibcoroEvent;
    DualMethodMap<S> m;

    m.emplace("set", DualNonBlockingMethod<S>{
                         [](S* s, void* args) { return s->Set(args); }});
    m.emplace("reset", DualNonBlockingMethod<S>{
                           [](S* s, void* args) { return s->Reset(args); }});
    m.emplace("is_set", DualNonBlockingMethod<S>{
                            [](S* s, void* args) { return s->IsSet(args); }});

    DualRequestMethod<S> wait_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestWait(op_id); };
    DualFollowUpMethod<S> wait_fol =
        [](S* s, void* /*args*/, int op_id) { return s->FollowUpWait(op_id); };

    m.emplace("wait", DualBlockingMethod<S>{wait_req, wait_fol});
    return m;
  }
};

struct LibcoroEventHash {
  size_t operator()(const LibcoroEvent& s) const {
    size_t h = std::hash<bool>{}(s.is_set);
    h ^= s.waiting.size() * 2654435761u;
    h ^= s.ready.size() * 97531u;
    return h;
  }
};

struct LibcoroEventEquals {
  bool operator()(const LibcoroEvent& a, const LibcoroEvent& b) const {
    return a.is_set == b.is_set &&
           a.waiting == b.waiting &&
           a.ready == b.ready;
  }
};

}  // namespace spec
