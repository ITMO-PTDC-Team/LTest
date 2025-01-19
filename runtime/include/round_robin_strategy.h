#pragma once

#include <utility>

#include "pick_strategy.h"

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct RoundRobinStrategy : PickStrategy<TargetObj, Verifier> {
  explicit RoundRobinStrategy(size_t threads_count,
                              std::vector<TaskBuilder> constructors)
      : next_task{0},
        PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors)} {}

  std::optional<size_t> Pick() override {
    auto &threads = PickStrategy<TargetObj, Verifier>::threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();
      if (!threads[cur].empty() && threads[cur].back()->IsBlocked()) {
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
      return cur;
    }
    return std::nullopt;
  }

  size_t next_task;
};
