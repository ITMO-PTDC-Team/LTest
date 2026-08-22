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

struct FollyCoroSharedMutexVerifierBase
    : ltest::DefaultStrategyTaskVerifier {
  bool Verify(const std::string& task_name, size_t thread_id, bool) {
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

  void OnFinished(ltest::Task& task, size_t thread_id) {
    EnsureThread(thread_id);
    const std::string task_name = std::string(task->GetName());
    const ltest::ValueWrapper result = task->GetRetVal();
    if (!result.HasValue()) {
      return;
    }

    if (task_name == "lock") {
      if (task->IsReturned()) {
        exclusive_owner_ = thread_id;
      }
      return;
    }

    if (task_name == "try_lock") {
      bool ok = result.GetValue<bool>();
      if (ok) {
        exclusive_owner_ = thread_id;
      }
      return;
    }

    if (task_name == "lock_shared") {
      if (task->IsReturned()) {
        ++shared_counts_[thread_id];
      }
      return;
    }

    if (task_name == "try_lock_shared") {
      bool ok = result.GetValue<bool>();
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

  void Reset() {
    exclusive_owner_.reset();
    shared_counts_.clear();
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
    ltest::ReservePolicyVerifier<spec::FollyCoroSharedMutex,
                                  FollyCoroSharedMutexVerifierBase>;

#endif  // LTEST_FOLLY_CORO_SHARED_MUTEX_VERIFIER_H
