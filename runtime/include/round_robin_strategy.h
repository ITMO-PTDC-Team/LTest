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
      if (!threads[cur].empty() && (threads[cur].back()->IsParked() ||
                                    threads[cur].back()->IsBlocked())) {
        continue;
      }
      return cur;
    }
    assert(false && "deadlock");
  }

  size_t next_task;
};
