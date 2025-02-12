#pragma once

#include <random>
#include <utility>

#include "pick_strategy.h"
#include "scheduler.h"

template <typename TargetObj>
struct RoundRobinStrategy : PickStrategy<TargetObj> {
  explicit RoundRobinStrategy(size_t threads_count,
                              std::vector<TaskBuilder> constructors)
      : next_task{0},
        PickStrategy<TargetObj>{threads_count, std::move(constructors)} {}

  size_t Pick() override {
    auto &threads = PickStrategy<TargetObj>::threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();
      if (!threads[cur].empty() && threads[cur].back()->IsParked()) {
        continue;
      }
      return cur;
    }
    assert(false && "deadlock");
  }

  size_t PickSchedule() override {
    auto &threads = PickStrategy<TargetObj>::threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();
      int task_index = this->GetNextTaskInThread(cur);

      if (
        task_index == threads[cur].size() ||
        threads[cur][task_index]->IsParked()
      ) {
        continue;
      }
      return cur;
    }
    assert(false && "deadlock");
  }

  size_t next_task;
};
