#pragma once
#include <random>
#include <utility>
#include <vector>

#include "lib.h"
#include "pick_strategy.h"

// Allows a random thread to work.
// Randoms new task.
template <typename TargetObj, StrategyVerifier Verifier>
struct RandomStrategy : PickStrategy<TargetObj, Verifier> {
  explicit RandomStrategy(size_t threads_count,
                          std::vector<TaskBuilder> constructors,
                          std::vector<int> weights)
      : PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors)},
        weights{std::move(weights)} {}

  size_t Pick() override {
    pick_weights.clear();
    auto &threads = PickStrategy<TargetObj, Verifier>::threads;
    for (size_t i = 0; i < threads.size(); ++i) {
      if (!threads[i].Empty() &&
          (threads[i].Back()->IsParked() || threads[i].Back()->IsBlocked())) {
        continue;
      }
      pick_weights.push_back(weights[i]);
    }

    assert(!pick_weights.empty() && "deadlock");

    auto thread_distribution =
        std::discrete_distribution<>(pick_weights.begin(), pick_weights.end());
    auto num = thread_distribution(PickStrategy<TargetObj, Verifier>::rng);
    for (size_t i = 0; i < threads.size(); ++i) {
      if (!threads[i].Empty() &&
          (threads[i].Back()->IsParked() || threads[i].Back()->IsBlocked())) {
        continue;
      }
      if (num == 0) {
        return i;
      }
      num--;
    }
    assert(false && "Cannot pick thread to continue round generation");
  }

  size_t PickSchedule() override {
    pick_weights.clear();
    auto &threads = this->threads;

    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].Size() ||
          threads[i][task_index]->IsParked()) {
        continue;
      }
      pick_weights.push_back(weights[i]);
    }

    assert(!pick_weights.empty() && "deadlock");

    auto thread_distribution =
        std::discrete_distribution<>(pick_weights.begin(), pick_weights.end());
    auto num = thread_distribution(this->rng);
    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].Size() ||
          threads[i][task_index]->IsParked()) {
        continue;
      }
      if (num == 0) {
        return i;
      }
      num--;
    }
    assert(false && "Cannot pick thread to continue round scheduling");
  }

 private:
  std::vector<int> weights;
  std::vector<int> pick_weights;
};
