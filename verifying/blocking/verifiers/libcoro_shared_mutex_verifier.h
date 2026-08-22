//
// Created by bitree.
//

#ifndef LTEST_LIBCORO_SHARED_MUTEX_VERIFIER_H
#define LTEST_LIBCORO_SHARED_MUTEX_VERIFIER_H

#pragma once

#include <optional>
#include <string>
#include <unordered_map>

#include "runtime/include/lib.h"
#include "runtime/include/scheduler.h"

struct LibcoroSharedMutexVerifier {
  enum : int32_t {
    FREE = 0,
    WRITER = 1,
    READER = 2,
  };

  bool Verify(const std::string& task_name, size_t thread_id, bool) {
    if (!status.contains(thread_id)) {
      status[thread_id] = FREE;
    }

    if (task_name == "lock") {
      return status[thread_id] == FREE;
    } else if (task_name == "lock_shared") {
      return status[thread_id] == FREE;
    } else if (task_name == "unlock") {
      return status[thread_id] == WRITER;
    } else if (task_name == "unlock_shared") {
      return status[thread_id] == READER;
    }

    assert(false && "unexpected method name in LibcoroSharedMutexVerifier");
    return false;
  }

  void OnFinished(ltest::Task& task, size_t thread_id) {
    const std::string task_name = std::string(task->GetName());

    if (task_name == "lock") {
      if (task->IsReturned()) {
        status[thread_id] = WRITER;
      }
      return;
    }

    if (task_name == "lock_shared") {
      if (task->IsReturned()) {
        status[thread_id] = READER;
      }
      return;
    }

    // IMPORTANT:
    // For cleanup progress, release methods must clear ownership even if
    // they finished during termination.
    if (task_name == "unlock") {
      status[thread_id] = FREE;
      return;
    }

    if (task_name == "unlock_shared") {
      status[thread_id] = FREE;
      return;
    }

    assert(false && "unexpected finished method name in LibcoroSharedMutexVerifier");
  }

  void Reset() { status.clear(); }

  std::unordered_map<size_t, size_t> status;
};

#endif  // LTEST_LIBCORO_SHARED_MUTEX_VERIFIER_H
