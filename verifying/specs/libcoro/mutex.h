//
// Created by bitree.
//

#ifndef LTEST_LIBCORO_MUTEX_H
#define LTEST_LIBCORO_MUTEX_H

#pragma once

#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

// Sequential mixed spec for coro::mutex:
// - lock()      : dual (request + follow-up)
// - try_lock()  : ordinary
// - unlock()    : ordinary
//
// Important: libcoro::mutex keeps waiters in a LIFO stack.
// unlock() transfers ownership to the newest waiter if one exists.
struct LibcoroMutex {
  bool locked = false;

  // waiting lock op_ids, LIFO
  std::vector<int> waiting_locks;

  // lock follow-up is ready for these ops
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
    if (!waiting_locks.empty()) {
      int op_id = waiting_locks.back();
      waiting_locks.pop_back();
      ready_locks.insert(op_id);

      // logical ownership is transferred directly to waiter,
      // so mutex remains logically locked.
      locked = true;
    } else {
      locked = false;
    }
    return void_v;
  }

  // ----- dual methods -----

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
    using S = LibcoroMutex;
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

struct LibcoroMutexHash {
  size_t operator()(const LibcoroMutex& s) const {
    size_t h = 0;
    h ^= static_cast<size_t>(s.locked) * 1315423911u;
    h ^= s.waiting_locks.size() * 2654435761u;
    h ^= s.ready_locks.size() * 97531u;
    return h;
  }
};

struct LibcoroMutexEquals {
  bool operator()(const LibcoroMutex& a, const LibcoroMutex& b) const {
    return a.locked == b.locked &&
           a.waiting_locks == b.waiting_locks &&
           a.ready_locks == b.ready_locks;
  }
};

}  // namespace spec

#endif  // LTEST_LIBCORO_MUTEX_H
