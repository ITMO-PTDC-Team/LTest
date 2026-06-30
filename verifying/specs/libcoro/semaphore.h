#pragma once

#include <algorithm>
#include <cstddef>
#include <type_traits>
#include <unordered_set>
#include <vector>

#include <coro/semaphore.hpp>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

template <>
inline ToStringFunc GetDefaultToString<coro::semaphore_acquire_result>() {
  return [](const ValueWrapper& a) -> std::string {
    return std::string("semaphore_acquire_result(") +
           coro::to_string(a.GetValue<coro::semaphore_acquire_result>()) + ")";
  };
}

template <>
inline CompFunc GetDefaultCompator<coro::semaphore_acquire_result>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<coro::semaphore_acquire_result>() ==
           b.GetValue<coro::semaphore_acquire_result>();
  };
}

namespace spec {

struct LibcoroSemaphore {
  static constexpr std::ptrdiff_t kMax = 2;
  static constexpr std::ptrdiff_t kInitial = 1;

  std::ptrdiff_t available = kInitial;
  bool shutdown = false;
  std::vector<int> waiting;
  std::unordered_set<int> ready_acquired;
  std::unordered_set<int> ready_shutdown;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;

    ltest::ReserveRule r;
    r.wait_method = "acquire";
    r.progress_methods = {"release", "shutdown"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));

    ltest::PrefixBudgetRule pb;
    pb.wait_method = "acquire";
    pb.progress_methods = {"release", "shutdown"};
    pb.reserve_threads = 1;
    pb.initial_credit = kInitial;
    p.prefix_budget.push_back(std::move(pb));

    return p;
  }

  ValueWrapper TryAcquire(void* /*args*/) {
    if (available <= 0) {
      return ValueWrapper(false);
    }

    --available;
    return ValueWrapper(true);
  }

  ValueWrapper Release(void* /*args*/) {
    if (available >= kMax) {
      return void_v;
    }

    if (!waiting.empty()) {
      int op_id = waiting.back();
      waiting.pop_back();
      ready_acquired.insert(op_id);
    } else {
      ++available;
    }

    return void_v;
  }

  ValueWrapper Shutdown(void* /*args*/) {
    if (shutdown) {
      return void_v;
    }

    shutdown = true;
    for (int op_id : waiting) {
      ready_shutdown.insert(op_id);
    }
    waiting.clear();
    return void_v;
  }

  ValueWrapper IsShutdown(void* /*args*/) {
    return ValueWrapper(shutdown);
  }

  ValueWrapper Value(void* /*args*/) {
    return ValueWrapper(static_cast<std::ptrdiff_t>(available));
  }

  ValueWrapper Max(void* /*args*/) {
    return ValueWrapper(static_cast<std::ptrdiff_t>(kMax));
  }

  void RequestAcquire(int op_id) {
    if (shutdown) {
      ready_shutdown.insert(op_id);
      return;
    }

    if (available > 0) {
      --available;
      ready_acquired.insert(op_id);
      return;
    }

    waiting.push_back(op_id);
  }

  std::optional<ValueWrapper> FollowUpAcquire(int op_id) {
    if (ready_acquired.contains(op_id)) {
      ready_acquired.erase(op_id);
      return ValueWrapper(coro::semaphore_acquire_result::acquired);
    }

    if (ready_shutdown.contains(op_id)) {
      ready_shutdown.erase(op_id);
      return ValueWrapper(coro::semaphore_acquire_result::shutdown);
    }

    return std::nullopt;
  }

  static auto GetDualMethods() {
    using S = LibcoroSemaphore;
    DualMethodMap<S> m;

    m.emplace("try_acquire", DualNonBlockingMethod<S>{
                                 [](S* s, void* args) {
                                   return s->TryAcquire(args);
                                 }});
    m.emplace("release", DualNonBlockingMethod<S>{
                             [](S* s, void* args) {
                               return s->Release(args);
                             }});
    m.emplace("shutdown", DualNonBlockingMethod<S>{
                              [](S* s, void* args) {
                                return s->Shutdown(args);
                              }});
    m.emplace("is_shutdown", DualNonBlockingMethod<S>{
                                 [](S* s, void* args) {
                                   return s->IsShutdown(args);
                                 }});
    m.emplace("value", DualNonBlockingMethod<S>{
                           [](S* s, void* args) {
                             return s->Value(args);
                           }});
    m.emplace("max", DualNonBlockingMethod<S>{
                         [](S* s, void* args) {
                           return s->Max(args);
                         }});

    DualRequestMethod<S> acquire_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestAcquire(op_id); };
    DualFollowUpMethod<S> acquire_fol =
        [](S* s, void* /*args*/, int op_id) {
          return s->FollowUpAcquire(op_id);
        };

    m.emplace("acquire", DualBlockingMethod<S>{acquire_req, acquire_fol});
    return m;
  }
};

struct LibcoroSemaphoreHash {
  size_t operator()(const LibcoroSemaphore& s) const {
    size_t h = std::hash<std::ptrdiff_t>{}(s.available);
    h ^= std::hash<bool>{}(s.shutdown) * 1315423911u;
    h ^= s.waiting.size() * 2654435761u;
    h ^= s.ready_acquired.size() * 97531u;
    h ^= s.ready_shutdown.size() * 433494437u;
    return h;
  }
};

struct LibcoroSemaphoreEquals {
  bool operator()(const LibcoroSemaphore& a,
                  const LibcoroSemaphore& b) const {
    return a.available == b.available &&
           a.shutdown == b.shutdown &&
           a.waiting == b.waiting &&
           a.ready_acquired == b.ready_acquired &&
           a.ready_shutdown == b.ready_shutdown;
  }
};

}  // namespace spec
