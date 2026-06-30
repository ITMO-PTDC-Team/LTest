//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_MUTEX_VERIFIER_H
#define LTEST_FOLLY_CORO_MUTEX_VERIFIER_H

#pragma once

#include <optional>
#include <string>

#include "runtime/include/lib.h"
#include "runtime/include/scheduler.h"
#include "runtime/include/strategy_verifier.h"
#include "../../../specs/folly/coro_mutex.h"

struct FollyCoroMutexVerifierBase : DefaultStrategyTaskVerifier {
  // We intentionally use an actor/thread-affine ownership model here.
  //
  // Why:
  // folly::coro::Mutex says ownership is not tied to an OS thread, but in this
  // LTEST target lock()/try_lock()/unlock() are separate top-level operations.
  // We are NOT testing one user coroutine migrating across executors while
  // holding the lock.
  //
  // So for this harness the most sound approximation is:
  //   - one logical owner actor at a time;
  //   - only owner may unlock;
  //   - an actor that already owns the mutex may not start lock/try_lock again.
  //
  // This matches blocking-style verifier design and is compatible with
  // scheduler cleanup ReleaseTask(thread_id).

  bool Verify(const std::string& task_name, size_t thread_id) {
    if (task_name == "lock" || task_name == "try_lock") {
      // Recursive acquire by the same actor is forbidden in this model.
      return !IsOwner(thread_id);
    } else if (task_name == "unlock") {
      // Only logical owner may unlock.
      return IsOwner(thread_id);
    }

    assert(false && "unexpected method name in FollyCoroMutexVerifier");
    return false;
  }

  void OnTaskStarted(const std::string& /*method*/,
                     std::size_t /*thread_id*/,
                     int /*task_id*/) {
    // Nothing to do.
    //
    // For this verifier we update ownership only on successful completion:
    // - try_lock() that returned true
    // - lock() that FinishedNormally()
    // - unlock()
  }

  void OnFinished(Task& task, size_t thread_id) {
    const std::string task_name = std::string(task->GetName());

    if (task_name == "lock") {
      // For dual lock(), ownership is acquired only after FOLLOWUP completes
      // and the task finishes normally.
      if (task->FinishedNormally()) {
        owner_thread_ = thread_id;
      }
      return;
    }

    if (task_name == "try_lock") {
      bool ok = task->GetRetVal().GetValue<bool>();
      if (ok) {
        owner_thread_ = thread_id;
      }
      return;
    }

    if (task_name == "unlock") {
      // In this verifier unlock() is only legal for owner, so after successful
      // completion the mutex becomes ownerless. If unlock() wakes a waiter,
      // that waiter will become owner later when its dual lock() finishes.
      if (IsOwner(thread_id)) {
        owner_thread_.reset();
      }
      return;
    }

    assert(false && "unexpected finished method name in FollyCoroMutexVerifier");
  }

  std::optional<std::string> ReleaseTask(size_t thread_id) {
    if (IsOwner(thread_id)) {
      return {"unlock"};
    }
    return std::nullopt;
  }

  void OnRoundStart(std::size_t /*threads*/) {
    owner_thread_.reset();
  }

 private:
  bool IsOwner(size_t thread_id) const {
    return owner_thread_.has_value() && *owner_thread_ == thread_id;
  }

 private:
  std::optional<size_t> owner_thread_;
};

using FollyCoroMutexVerifier =
    ReservePolicyVerifier<spec::FollyCoroMutex, FollyCoroMutexVerifierBase>;

#endif  // LTEST_FOLLY_CORO_MUTEX_VERIFIER_H
