#pragma once

#include <utility>

#include "pick_strategy.h"

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct RoundRobinStrategy : PickStrategy<TargetObj, Verifier> {
  using TargetFactory =
      typename PickStrategy<TargetObj, Verifier>::TargetFactory;

  explicit RoundRobinStrategy(size_t threads_count,
                              std::vector<TaskBuilder> constructors,
                              TargetFactory target_factory,
                              size_t seed = 0)
      : next_task{0},
        PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors),
                                          std::move(target_factory),
                                          seed} {}

  std::optional<size_t> Pick() override {
    auto &threads = PickStrategy<TargetObj, Verifier>::threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();

      const bool is_free = threads[cur].empty() || threads[cur].back()->IsReturned();
      if (!this->AllowNewTasks() && is_free) continue;

      if (!threads[cur].empty() && threads[cur].back()->IsBlocked()) {
        continue;
      }
      if (!is_free && !this->VerifyExistingTask(threads[cur].back(), cur)) {
        continue;
      }
      return cur;
    }
    return std::nullopt;
  }

  std::optional<size_t> PickSchedule() override {
    auto &threads = this->threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();
      int task_index = this->GetNextTaskInThread(cur);

      if (task_index == threads[cur].size() ||
          threads[cur][task_index]->IsBlocked()) {
        continue;
      }
      if (!this->VerifyExistingTask(threads[cur][task_index], cur)) {
        continue;
      }
      return cur;
    }
    return std::nullopt;
  }

  size_t next_task;
};
