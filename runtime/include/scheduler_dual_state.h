//
// Created by d84370027 on 1/26/2026.
//

#ifndef SCHEDULER_DUAL_STATE_H
#define SCHEDULER_DUAL_STATE_H

#pragma once
#include <mutex>
#include <unordered_map>

// храню все как просили
enum class DualPhase { NONE, REQUEST, FOLLOWUP };

class DualStateTracker {
 private:
  // task_id -> count resume
  std::unordered_map<int, int> resume_count_;
  // task_id -> phase
  std::unordered_map<int, DualPhase> task_phase_;
  std::mutex mutex_;

 public:
  void RegisterTask(int task_id) {
    std::lock_guard lock(mutex_);
    resume_count_[task_id] = 0;
    task_phase_[task_id] = DualPhase::NONE;
  }

  int IncrementResumeCount(int task_id) {
    std::lock_guard lock(mutex_);
    int count = ++resume_count_[task_id];

    if (count == 1) {
      task_phase_[task_id] = DualPhase::REQUEST;
    } else {
      task_phase_[task_id] = DualPhase::FOLLOWUP;
    }

    return count;
  }

  DualPhase GetPhase(int task_id) const {
    std::lock_guard lock(mutex_);
    auto it = task_phase_.find(task_id);
    if (it != task_phase_.end()) {
      return it->second;
    }
    return DualPhase::NONE;
  }

  void ResetTask(int task_id) {
    std::lock_guard lock(mutex_);
    resume_count_.erase(task_id);
    task_phase_.erase(task_id);
  }

  bool IsDualOperation(int task_id) const {
    std::lock_guard lock(mutex_);
    auto it = resume_count_.find(task_id);
    if (it != resume_count_.end()) {
      return it->second > 1;
    }
    return false;
  }

  int GetResumeCount(int task_id) const {
    std::lock_guard lock(mutex_);
    auto it = resume_count_.find(task_id);
    if (it != resume_count_.end()) {
      return it->second;
    }
    return 0;
  }
};

#endif  // SCHEDULER_DUAL_STATE_H
