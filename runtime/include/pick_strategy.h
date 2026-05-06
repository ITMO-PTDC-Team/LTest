
#pragma once
#include <algorithm>
#include <memory>
#include <random>

#include "scheduler.h"

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct PickStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  virtual std::optional<size_t> Pick() = 0;

  virtual std::optional<size_t> PickSchedule() = 0;

  using TargetFactory =
      typename BaseStrategyWithThreads<TargetObj, Verifier>::TargetFactory;

  explicit PickStrategy(size_t threads_count,
                        std::vector<TaskBuilder> constructors,
                        TargetFactory target_factory,
                        size_t seed = 0)
      : BaseStrategyWithThreads<TargetObj, Verifier>(threads_count,
                                                     std::move(constructors),
                                                     std::move(target_factory),
                                                     seed) {}

  std::optional<size_t> NextThreadId() override { return Pick(); }

  std::optional<TaskWithMetaData> NextSchedule() override {
    auto& round_schedule = this->round_schedule;
    auto current_thread_opt = PickSchedule();
    if (!current_thread_opt.has_value()) {
      return std::nullopt;
    }
    size_t current_thread = current_thread_opt.value();
    int next_task_index = this->GetNextTaskInThread(current_thread);
    bool is_new = round_schedule[current_thread] != next_task_index;

    round_schedule[current_thread] = next_task_index;
    return TaskWithMetaData{this->threads[current_thread][next_task_index],
                            is_new, current_thread};
  }

  void StartNextRound() override {
    this->SetAllowNewTasks(true);
    this->new_task_id = 0;

    this->AbortForRoundReset();
    if (this->threads.size() != this->threads_count) {
      this->threads.clear();
      this->threads.resize(this->threads_count);
    } else {
      for (auto& thread : this->threads) {
        while (thread.size() > 0) {
          thread.pop_back();
        }
      }
    }
    this->round_schedule.assign(this->threads_count, -1);
    this->removed_tasks.clear();

    ltest::verifier_hooks::OnRoundStart(this->sched_checker, this->threads_count);
  }

  ~PickStrategy() {
    this->TerminateTasks();
  }


 protected:
  size_t next_task = 0;
};
