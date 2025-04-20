#pragma once

#include <utility>

#include "pick_strategy.h"

template <typename TargetObj, StrategyVerifier Verifier>
struct RoundRobinStrategy : PickStrategy<TargetObj, Verifier> {
  explicit RoundRobinStrategy(size_t threads_count,
                              std::vector<TaskBuilder> constructors)
      : next_task{0},
        PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors)} {}

  size_t Pick() override {
    auto &threads = PickStrategy<TargetObj, Verifier>::threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();
      if (!threads[cur].Empty() && (threads[cur].Back()->IsParked() ||
                                    threads[cur].Back()->IsBlocked())) {
        continue;
      }
      return cur;
    }
    assert(false && "deadlock");
  }

  size_t PickSchedule() override {
    auto &threads = this->threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();
      int task_index = this->GetNextTaskInThread(cur);

      if (task_index == threads[cur].Size() ||
          threads[cur][task_index]->IsParked()) {
        continue;
      }
      return cur;
    }
    assert(false && "deadlock");
  }

  size_t next_task;
};
