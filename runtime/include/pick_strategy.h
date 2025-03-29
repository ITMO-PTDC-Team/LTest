
#pragma once
#include <algorithm>
#include <memory>
#include <random>

#include "scheduler.h"

template <typename TargetObj, StrategyVerifier Verifier>
struct PickStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  virtual size_t Pick() = 0;

  virtual size_t PickSchedule() = 0;

  explicit PickStrategy(size_t threads_count,
                        std::vector<TaskBuilder> constructors)
      : BaseStrategyWithThreads<TargetObj, Verifier>(threads_count, constructors) {
  }

  size_t NextThreadId() override {
    return Pick();
  }

  TaskWithMetaData NextSchedule() override {
    auto& round_schedule = this->round_schedule;
    size_t current_thread = PickSchedule();
    int next_task_index = this->GetNextTaskInThread(current_thread);
    bool is_new = round_schedule[current_thread] != next_task_index;

    round_schedule[current_thread] = next_task_index;
    return TaskWithMetaData{this->threads[current_thread][next_task_index],
                            is_new, current_thread};
  }

  void StartNextRound() override {
    this->new_task_id = 0;

    this->TerminateTasks();
    for (auto& thread : this->threads) {
      // We don't have to keep references alive
      while (thread.size() > 0) {
        thread.pop_back();
      }
    }
  }

  ~PickStrategy() { this->TerminateTasks(); }

 protected:
  size_t next_task = 0;
  size_t threads_count;
};
