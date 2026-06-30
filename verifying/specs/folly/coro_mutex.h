//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_MUTEX_H
#define LTEST_FOLLY_CORO_MUTEX_H
#pragma once

#include <deque>
#include <optional>
#include <string>
#include <unordered_set>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

// Mixed sequential spec for folly::coro::Mutex:
// - lock()      : dual
// - try_lock()  : ordinary
// - unlock()    : ordinary
//
// Folly Mutex guarantees FIFO scheduling of waiters.
struct FollyCoroMutex {
  bool locked = false;

  // FIFO queue of waiting lock operations
  std::deque<int> waiting_locks;

  // lock follow-up is ready for these operations
  std::unordered_set<int> ready_locks;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;
    ltest::ReserveRule r;
    r.wait_method = "lock";
    r.progress_methods = {"unlock"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));
    return p;
  }

  // ----- ordinary methods -----

  ValueWrapper TryLock(void* /*args*/) {
    if (!locked) {
      locked = true;
      return ValueWrapper(true);
    }
    return ValueWrapper(false);
  }

  ValueWrapper Unlock(void* /*args*/) {
    // Protocol legality is handled by verifier.
    if (!waiting_locks.empty()) {
      int op_id = waiting_locks.front();
      waiting_locks.pop_front();
      ready_locks.insert(op_id);

      // Ownership transfers directly to the next waiter.
      locked = true;
    } else {
      locked = false;
    }
    return void_v;
  }

  // ----- dual lock -----

  void RequestLock(int op_id) {
    if (!locked) {
      locked = true;
      ready_locks.insert(op_id);
    } else {
      waiting_locks.push_back(op_id);
    }
  }

  std::optional<ValueWrapper> FollowUpLock(int op_id) {
    if (!ready_locks.contains(op_id)) {
      return std::nullopt;
    }
    ready_locks.erase(op_id);
    return void_v;
  }

  static auto GetDualMethods() {
    using S = FollyCoroMutex;
    DualMethodMap<S> m;

    DualNonBlockingMethod<S> try_lock =
        [](S* s, void* args) -> ValueWrapper {
      return s->TryLock(args);
    };

    DualNonBlockingMethod<S> unlock =
        [](S* s, void* args) -> ValueWrapper {
      return s->Unlock(args);
    };

    DualRequestMethod<S> lock_req =
        [](S* s, void* /*args*/, int op_id) {
      s->RequestLock(op_id);
    };

    DualFollowUpMethod<S> lock_fol =
        [](S* s, void* /*args*/, int op_id) -> std::optional<ValueWrapper> {
      return s->FollowUpLock(op_id);
    };

    m.emplace("try_lock", try_lock);
    m.emplace("unlock", unlock);
    m.emplace("lock", DualBlockingMethod<S>{lock_req, lock_fol});

    return m;
  }
};

struct FollyCoroMutexHash {
  size_t operator()(const FollyCoroMutex& s) const {
    size_t h = 0;
    h ^= static_cast<size_t>(s.locked) * 1315423911u;
    h ^= s.waiting_locks.size() * 2654435761u;
    h ^= s.ready_locks.size() * 97531u;
    return h;
  }
};

struct FollyCoroMutexEquals {
  bool operator()(const FollyCoroMutex& a, const FollyCoroMutex& b) const {
    return a.locked == b.locked &&
           a.waiting_locks == b.waiting_locks &&
           a.ready_locks == b.ready_locks;
  }
};

}  // namespace spec

#endif  // LTEST_FOLLY_CORO_MUTEX_H
