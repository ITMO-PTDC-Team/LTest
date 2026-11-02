#pragma once

#include <utility>

#include "pick_strategy.h"

namespace ltest {

struct RoundRobinSharedState : StrategySharedState<RoundRobinSharedState> {
  size_t next_task = 0;
};

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct RoundRobinStrategy : PickStrategy<TargetObj, Verifier> {
 private:
  MappedStrategyState<RoundRobinSharedState> shared_state;

 public:
  using TargetFactory =
      typename PickStrategy<TargetObj, Verifier>::TargetFactory;

  explicit RoundRobinStrategy(size_t threads_count,
                              std::vector<TaskBuilder> constructors,
                              TargetFactory target_factory)
      : PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors),
                                          std::move(target_factory)} {}

  RoundRobinSharedState& GetSharedState() override { return *shared_state; }

  std::optional<size_t> Pick() override {
    auto& threads = this->threads;
    for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
      auto cur = (next_task++) % threads.size();

      const bool is_free = threads[cur].empty() || threads[cur].back()->IsReturned();
      if (!this->AllowNewTasks() && is_free) continue;

      if ((!threads[cur].empty() && threads[cur].back()->IsBlocked()) ||
          !this->CanThreadContinue(cur) ||
          !this->NewTaskAreEnabled(cur)) {
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

      if (task_index == threads[cur].size() || !this->CanThreadContinue(cur) ||
          threads[cur][task_index]->IsBlocked()) {
        continue;
      }
      return cur;
    }
    return std::nullopt;
  }

  size_t& next_task = this->GetSharedState().next_task;
};

}  // namespace ltest
