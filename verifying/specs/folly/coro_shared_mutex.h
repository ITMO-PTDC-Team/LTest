//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_SHARED_MUTEX_H
#define LTEST_FOLLY_CORO_SHARED_MUTEX_H

#pragma once

#include <deque>
#include <optional>
#include <string>
#include <unordered_set>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace spec {

// Sequential mixed spec for folly::coro::SharedMutexFair.
//
// The async lock methods are dual. try_lock/try_lock_shared/unlock operations
// are ordinary. The waiter queue is FIFO; an exclusive waiter blocks later
// shared waiters until it acquires the lock.
struct FollyCoroSharedMutex {
  int active_readers = 0;
  bool writer_active = false;

  struct Waiter {
    int op_id;
    bool exclusive;

    bool operator==(const Waiter&) const = default;
  };

  std::deque<Waiter> waiters;
  std::unordered_set<int> ready_locks;
  std::unordered_set<int> ready_shared_locks;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;

    ltest::ReserveRule lock_wait;
    lock_wait.wait_method = "lock";
    lock_wait.progress_methods = {"unlock", "unlock_shared"};
    lock_wait.reserve_threads = 1;
    p.reserve.push_back(std::move(lock_wait));

    ltest::ReserveRule shared_wait;
    shared_wait.wait_method = "lock_shared";
    shared_wait.progress_methods = {"unlock", "unlock_shared"};
    shared_wait.reserve_threads = 1;
    p.reserve.push_back(std::move(shared_wait));

    return p;
  }

  ValueWrapper TryLock(void* /*args*/) {
    if (CanTryLock()) {
      writer_active = true;
      return ValueWrapper(true);
    }
    return ValueWrapper(false);
  }

  bool CheckTryLock(void* args, const ValueWrapper* result) {
    if (result != nullptr && !result->GetValue<bool>()) {
      return true;
    }
    return TryLock(args).GetValue<bool>();
  }

  ValueWrapper TryLockShared(void* /*args*/) {
    if (CanTryLockShared()) {
      ++active_readers;
      return ValueWrapper(true);
    }
    return ValueWrapper(false);
  }

  bool CheckTryLockShared(void* args, const ValueWrapper* result) {
    if (result != nullptr && !result->GetValue<bool>()) {
      return true;
    }
    return TryLockShared(args).GetValue<bool>();
  }

  ValueWrapper Unlock(void* /*args*/) {
    writer_active = false;
    WakeNextWaiters();
    return void_v;
  }

  ValueWrapper UnlockShared(void* /*args*/) {
    if (active_readers > 0) {
      --active_readers;
    }
    if (active_readers == 0) {
      WakeNextWaiters();
    }
    return void_v;
  }

  void RequestLock(int op_id) {
    if (!writer_active && active_readers == 0 && waiters.empty() &&
        ready_locks.empty() && ready_shared_locks.empty()) {
      ready_locks.insert(op_id);
    } else {
      waiters.push_back(Waiter{op_id, true});
    }
  }

  std::optional<ValueWrapper> FollowUpLock(int op_id) {
    if (!ready_locks.contains(op_id) || writer_active ||
        active_readers != 0) {
      return std::nullopt;
    }
    ready_locks.erase(op_id);
    writer_active = true;
    return void_v;
  }

  void RequestLockShared(int op_id) {
    if (!writer_active) {
      ready_shared_locks.insert(op_id);
    } else {
      waiters.push_back(Waiter{op_id, false});
    }
  }

  std::optional<ValueWrapper> FollowUpLockShared(int op_id) {
    if (!ready_shared_locks.contains(op_id) || writer_active) {
      return std::nullopt;
    }
    ready_shared_locks.erase(op_id);
    ++active_readers;
    return void_v;
  }

  static auto GetDualMethods() {
    using S = FollyCoroSharedMutex;
    DualMethodMap<S> m;

    m.emplace("try_lock",
              DualNonBlockingPredicateMethod<S>{
                  [](S* s, void* args, const ValueWrapper* result) {
                    return s->CheckTryLock(args, result);
                  }});
    m.emplace("try_lock_shared",
              DualNonBlockingPredicateMethod<S>{
                  [](S* s, void* args, const ValueWrapper* result) {
                    return s->CheckTryLockShared(args, result);
                  }});
    m.emplace("unlock", DualNonBlockingMethod<S>{
                            [](S* s, void* args) { return s->Unlock(args); }});
    m.emplace("unlock_shared",
              DualNonBlockingMethod<S>{
                  [](S* s, void* args) { return s->UnlockShared(args); }});

    DualRequestMethod<S> lock_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestLock(op_id); };
    DualFollowUpMethod<S> lock_fol =
        [](S* s, void* /*args*/, int op_id) { return s->FollowUpLock(op_id); };
    m.emplace("lock", DualBlockingMethod<S>{lock_req, lock_fol});

    DualRequestMethod<S> lock_shared_req =
        [](S* s, void* /*args*/, int op_id) { s->RequestLockShared(op_id); };
    DualFollowUpMethod<S> lock_shared_fol =
        [](S* s, void* /*args*/, int op_id) {
          return s->FollowUpLockShared(op_id);
        };
    m.emplace("lock_shared",
              DualBlockingMethod<S>{lock_shared_req, lock_shared_fol});

    return m;
  }

 private:
  bool CanTryLock() const {
    return !writer_active && active_readers == 0 && waiters.empty() &&
           ready_locks.empty() && ready_shared_locks.empty();
  }

  bool CanTryLockShared() const {
    return !writer_active && ready_locks.empty();
  }

  void WakeNextWaiters() {
    if (writer_active || active_readers != 0 || !ready_locks.empty() ||
        !ready_shared_locks.empty() || waiters.empty()) {
      return;
    }

    if (waiters.front().exclusive) {
      int op_id = waiters.front().op_id;
      waiters.pop_front();
      ready_locks.insert(op_id);
      return;
    }

    while (!waiters.empty() && !waiters.front().exclusive) {
      int op_id = waiters.front().op_id;
      waiters.pop_front();
      ready_shared_locks.insert(op_id);
    }
  }
};

struct FollyCoroSharedMutexHash {
  size_t operator()(const FollyCoroSharedMutex& s) const {
    size_t h = 0;
    h ^= static_cast<size_t>(s.active_readers) * 1315423911u;
    h ^= static_cast<size_t>(s.writer_active) * 2654435761u;
    for (const auto& waiter : s.waiters) {
      h ^= static_cast<size_t>(waiter.op_id + 1) * 97531u;
      h ^= static_cast<size_t>(waiter.exclusive) * 424242u;
      h = (h << 6) ^ (h >> 2) ^ 0x9e3779b97f4a7c15ull;
    }
    for (int op_id : s.ready_locks) {
      h ^= static_cast<size_t>(op_id + 1) * 65537u;
    }
    for (int op_id : s.ready_shared_locks) {
      h ^= static_cast<size_t>(op_id + 1) * 2166136261u;
    }
    return h;
  }
};

struct FollyCoroSharedMutexEquals {
  bool operator()(const FollyCoroSharedMutex& a,
                  const FollyCoroSharedMutex& b) const {
    return a.active_readers == b.active_readers &&
           a.writer_active == b.writer_active &&
           a.waiters == b.waiters &&
           a.ready_locks == b.ready_locks &&
           a.ready_shared_locks == b.ready_shared_locks;
  }
};

}  // namespace spec

#endif  // LTEST_FOLLY_CORO_SHARED_MUTEX_H
