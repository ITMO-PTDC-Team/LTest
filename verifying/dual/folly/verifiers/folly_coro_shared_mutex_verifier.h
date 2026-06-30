//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_SHARED_MUTEX_VERIFIER_H
#define LTEST_FOLLY_CORO_SHARED_MUTEX_VERIFIER_H

#pragma once

#include <optional>
#include <string>
#include <vector>

#include "runtime/include/lib.h"
#include "runtime/include/scheduler.h"
#include "runtime/include/strategy_verifier.h"
#include "../../../specs/folly/coro_shared_mutex.h"

struct FollyCoroSharedMutexVerifierBase : DefaultStrategyTaskVerifier {
  bool Verify(const std::string& task_name, size_t thread_id) {
    EnsureThread(thread_id);

    if (task_name == "lock" || task_name == "try_lock") {
      return !IsExclusiveOwner(thread_id) && shared_counts_[thread_id] == 0;
    }
    if (task_name == "lock_shared" || task_name == "try_lock_shared") {
      return !IsExclusiveOwner(thread_id) && shared_counts_[thread_id] == 0;
    }
    if (task_name == "unlock") {
      return IsExclusiveOwner(thread_id);
    }
    if (task_name == "unlock_shared") {
      return shared_counts_[thread_id] > 0;
    }

    assert(false && "unexpected method name in FollyCoroSharedMutexVerifier");
    return false;
  }

  void OnTaskStarted(const std::string& /*method*/,
                     std::size_t /*thread_id*/,
                     int /*task_id*/) {}

  void OnFinished(Task& task, size_t thread_id) {
    EnsureThread(thread_id);
    const std::string task_name = std::string(task->GetName());

    if (task_name == "lock") {
      if (task->FinishedNormally()) {
        exclusive_owner_ = thread_id;
      }
      return;
    }

    if (task_name == "try_lock") {
      bool ok = task->GetRetVal().GetValue<bool>();
      if (ok) {
        exclusive_owner_ = thread_id;
      }
      return;
    }

    if (task_name == "lock_shared") {
      if (task->FinishedNormally()) {
        ++shared_counts_[thread_id];
      }
      return;
    }

    if (task_name == "try_lock_shared") {
      bool ok = task->GetRetVal().GetValue<bool>();
      if (ok) {
        ++shared_counts_[thread_id];
      }
      return;
    }

    if (task_name == "unlock") {
      if (IsExclusiveOwner(thread_id)) {
        exclusive_owner_.reset();
      }
      return;
    }

    if (task_name == "unlock_shared") {
      if (shared_counts_[thread_id] > 0) {
        --shared_counts_[thread_id];
      }
      return;
    }

    assert(false && "unexpected finished method name in "
                    "FollyCoroSharedMutexVerifier");
  }

  std::optional<std::string> ReleaseTask(size_t thread_id) {
    EnsureThread(thread_id);
    if (IsExclusiveOwner(thread_id)) {
      return {"unlock"};
    }
    if (shared_counts_[thread_id] > 0) {
      return {"unlock_shared"};
    }
    return std::nullopt;
  }

  void OnRoundStart(std::size_t threads) {
    exclusive_owner_.reset();
    shared_counts_.assign(threads, 0);
  }

 private:
  void EnsureThread(size_t thread_id) {
    if (thread_id >= shared_counts_.size()) {
      shared_counts_.resize(thread_id + 1, 0);
    }
  }

  bool IsExclusiveOwner(size_t thread_id) const {
    return exclusive_owner_.has_value() && *exclusive_owner_ == thread_id;
  }

 private:
  std::optional<size_t> exclusive_owner_;
  std::vector<int> shared_counts_;
};

using FollyCoroSharedMutexVerifier =
    ReservePolicyVerifier<spec::FollyCoroSharedMutex,
                          FollyCoroSharedMutexVerifierBase>;

#endif  // LTEST_FOLLY_CORO_SHARED_MUTEX_VERIFIER_H
