#pragma once

#include <cassert>
#include <optional>
#include <string>

#include "runtime/include/strategy_verifier.h"
#include "verifying/specs/libcoro/mutex.h"

struct LibcoroMutexVerifierBase : ltest::DefaultStrategyTaskVerifier {
  bool Verify(const std::string& task_name, size_t thread_id, bool) {
    if (task_name == "lock" || task_name == "try_lock") {
      return !IsOwner(thread_id);
    }

    if (task_name == "unlock") {
      return IsOwner(thread_id);
    }

    assert(false && "unexpected method name in LibcoroMutexVerifier");
    return false;
  }

  void OnFinished(ltest::Task& task, size_t thread_id) {
    const std::string task_name = std::string(task->GetName());

    if (task_name == "lock") {
      if (task->IsReturned()) {
        owner_thread_ = thread_id;
      }
      return;
    }

    if (task_name == "try_lock") {
      if (task->GetRetVal().GetValue<bool>()) {
        owner_thread_ = thread_id;
      }
      return;
    }

    if (task_name == "unlock") {
      if (IsOwner(thread_id)) {
        owner_thread_.reset();
      }
      return;
    }

    assert(false && "unexpected finished method name in LibcoroMutexVerifier");
  }

 private:
  bool IsOwner(size_t thread_id) const {
    return owner_thread_.has_value() && *owner_thread_ == thread_id;
  }

  std::optional<size_t> owner_thread_;
};

using LibcoroMutexVerifier =
    ltest::ReservePolicyVerifier<spec::LibcoroMutex, LibcoroMutexVerifierBase>;
