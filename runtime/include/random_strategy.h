#pragma once
#include <random>
#include <utility>
#include <vector>

#include "lib.h"
#include "pick_strategy.h"

// Allows a random thread to work.
// Randoms new task.
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct RandomStrategy : PickStrategy<TargetObj, Verifier> {
  explicit RandomStrategy(size_t threads_count,
                          std::vector<TaskBuilder> constructors,
                          std::vector<int> weights)
      : PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors)},
        weights{std::move(weights)} {}

  std::optional<size_t> Pick() override {
    pick_weights.clear();
    auto &threads = PickStrategy<TargetObj, Verifier>::threads;
    for (size_t i = 0; i < threads.size(); ++i) {
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) {
        continue;
      }
      pick_weights.push_back(weights[i]);
    }

    if (pick_weights.empty()) [[unlikely]] {
      return std::nullopt;
    }

    auto thread_distribution =
        std::discrete_distribution<>(pick_weights.begin(), pick_weights.end());
    auto num = thread_distribution(PickStrategy<TargetObj, Verifier>::rng);
    for (size_t i = 0; i < threads.size(); ++i) {
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) {
        continue;
      }
      if (num == 0) {
        return i;
      }
      num--;
    }
    return std::nullopt;
  }

  std::optional<size_t> PickSchedule() override {
    pick_weights.clear();
    auto &threads = this->threads;

    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].size() ||
          threads[i][task_index]->IsBlocked()) {
        continue;
      }
      pick_weights.push_back(weights[i]);
    }

    if (pick_weights.empty()) [[unlikely]] {
      return std::nullopt;
    }

    auto thread_distribution =
        std::discrete_distribution<>(pick_weights.begin(), pick_weights.end());
    auto num = thread_distribution(this->rng);
    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].size() ||
          threads[i][task_index]->IsBlocked()) {
        continue;
      }
      if (num == 0) {
        return i;
      }
      num--;
    }
    return std::nullopt;
  }

 private:
  std::vector<int> weights;
  std::vector<int> pick_weights;
};
