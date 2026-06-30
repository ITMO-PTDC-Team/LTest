//
// Created by bitree.
//

#ifndef LTEST_LIBCORO_SHARED_MUTEX_H
#define LTEST_LIBCORO_SHARED_MUTEX_H

#pragma once

#include <cstdint>
#include <deque>
#include <optional>
#include <unordered_set>
#include <utility>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"

namespace spec {

struct LibcoroSharedMutex {
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

  bool HasExclusiveWaiter() const {
    for (const auto& waiter : waiters) {
      if (waiter.exclusive) {
        return true;
      }
    }
    return false;
  }

  void WakeNextWaiters() {
    if (writer_active || active_readers != 0 || waiters.empty()) {
      return;
    }

    if (waiters.front().exclusive) {
      int op_id = waiters.front().op_id;
      waiters.pop_front();
      writer_active = true;
      ready_locks.insert(op_id);
      return;
    }

    while (!waiters.empty() && !waiters.front().exclusive) {
      int op_id = waiters.front().op_id;
      waiters.pop_front();
      ++active_readers;
      ready_shared_locks.insert(op_id);
    }
  }

  // lock()

  void RequestLock(int op_id) {
    if (!writer_active && active_readers == 0) {
      writer_active = true;
      ready_locks.insert(op_id);
      return;
    }

    waiters.push_back(Waiter{op_id, true});
  }

  std::optional<ValueWrapper> FollowUpLock(int op_id) {
    if (!ready_locks.contains(op_id)) {
      return std::nullopt;
    }

    ready_locks.erase(op_id);
    return void_v;
  }

  // lock_shared()

  void RequestLockShared(int op_id) {
    if (!writer_active && !HasExclusiveWaiter()) {
      ++active_readers;
      ready_shared_locks.insert(op_id);
      return;
    }

    waiters.push_back(Waiter{op_id, false});
  }

  std::optional<ValueWrapper> FollowUpLockShared(int op_id) {
    if (!ready_shared_locks.contains(op_id)) {
      return std::nullopt;
    }

    ready_shared_locks.erase(op_id);
    return void_v;
  }

  // unlock()

  ValueWrapper Unlock(void* /*args*/) {
    writer_active = false;
    WakeNextWaiters();
    return void_v;
  }

  // unlock_shared()

  ValueWrapper UnlockShared(void* /*args*/) {
    if (active_readers > 0) {
      --active_readers;
    }

    if (active_readers == 0) {
      WakeNextWaiters();
    }

    return void_v;
  }

  static auto GetDualMethods() {
    using S = LibcoroSharedMutex;
    DualMethodMap<S> m;

    DualRequestMethod<S> lock_req =
        [](S* s, void*, int op_id) { s->RequestLock(op_id); };
    DualFollowUpMethod<S> lock_fol =
        [](S* s, void*, int op_id) { return s->FollowUpLock(op_id); };

    DualRequestMethod<S> lock_shared_req =
        [](S* s, void*, int op_id) { s->RequestLockShared(op_id); };
    DualFollowUpMethod<S> lock_shared_fol =
        [](S* s, void*, int op_id) { return s->FollowUpLockShared(op_id); };

    DualNonBlockingMethod<S> unlock =
        [](S* s, void* args) { return s->Unlock(args); };
    DualNonBlockingMethod<S> unlock_shared =
        [](S* s, void* args) { return s->UnlockShared(args); };

    m.emplace("lock", DualBlockingMethod<S>{lock_req, lock_fol});
    m.emplace("lock_shared",
              DualBlockingMethod<S>{lock_shared_req, lock_shared_fol});
    m.emplace("unlock", unlock);
    m.emplace("unlock_shared", unlock_shared);

    return m;
  }
};

struct LibcoroSharedMutexHash {
  size_t operator()(const LibcoroSharedMutex& s) const {
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

struct LibcoroSharedMutexEquals {
  bool operator()(const LibcoroSharedMutex& a,
                  const LibcoroSharedMutex& b) const {
    return a.active_readers == b.active_readers &&
           a.writer_active == b.writer_active &&
           a.waiters == b.waiters &&
           a.ready_locks == b.ready_locks &&
           a.ready_shared_locks == b.ready_shared_locks;
  }
};

}  // namespace spec

#endif  // LTEST_LIBCORO_SHARED_MUTEX_H
