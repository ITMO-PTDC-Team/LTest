#pragma once

#include <cstdint>
#include <tuple>
#include <unordered_set>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

#include <coro/latch.hpp>

namespace spec {

struct LibcoroLatch {
  static constexpr std::int64_t kInitial = 2;

  std::int64_t remaining = kInitial;
  std::unordered_set<int> waiting;
  std::unordered_set<int> ready;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;

    ltest::ReserveRule r;
    r.wait_method = "wait";
    r.progress_methods = {"count_down"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));

    return p;
  }

  ValueWrapper CountDown(void* args) {
    auto* tup = reinterpret_cast<std::tuple<std::int64_t>*>(args);
    remaining -= std::get<0>(*tup);
    if (remaining <= 0) {
      for (int op_id : waiting) {
        ready.insert(op_id);
      }
      waiting.clear();
    }
    return void_v;
  }

  ValueWrapper Remaining(void* /*args*/) {
    return ValueWrapper(static_cast<std::size_t>(remaining));
  }

  ValueWrapper IsReady(void* /*args*/) {
    return ValueWrapper(remaining <= 0);
  }

  void RequestWait(int op_id) {
    if (remaining <= 0) {
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
    using S = LibcoroLatch;
    DualMethodMap<S> m;

    m.emplace("count_down", DualNonBlockingMethod<S>{
                                [](S* s, void* args) {
                                  return s->CountDown(args);
                                }});
    m.emplace("remaining", DualNonBlockingMethod<S>{
                               [](S* s, void* args) {
                                 return s->Remaining(args);
                               }});
    m.emplace("is_ready", DualNonBlockingMethod<S>{
                              [](S* s, void* args) {
                                return s->IsReady(args);
                              }});

    DualRequestMethod<S> wait_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestWait(op_id); };
    DualFollowUpMethod<S> wait_fol =
        [](S* s, void* /*args*/, int op_id) { return s->FollowUpWait(op_id); };

    m.emplace("wait", DualBlockingMethod<S>{wait_req, wait_fol});
    return m;
  }
};

struct LibcoroLatchHash {
  size_t operator()(const LibcoroLatch& s) const {
    size_t h = std::hash<std::int64_t>{}(s.remaining);
    h ^= s.waiting.size() * 2654435761u;
    h ^= s.ready.size() * 97531u;
    return h;
  }
};

struct LibcoroLatchEquals {
  bool operator()(const LibcoroLatch& a, const LibcoroLatch& b) const {
    return a.remaining == b.remaining &&
           a.waiting == b.waiting &&
           a.ready == b.ready;
  }
};

}  // namespace spec
