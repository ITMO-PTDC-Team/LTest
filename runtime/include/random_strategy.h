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
  using TargetFactory =
      typename PickStrategy<TargetObj, Verifier>::TargetFactory;

  explicit RandomStrategy(size_t threads_count,
                          std::vector<TaskBuilder> constructors,
                          std::vector<int> weights,
                          TargetFactory target_factory,
                          size_t seed = 0)
      : PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors),
                                          std::move(target_factory),
                                          seed},
        weights{std::move(weights)} {}

  std::optional<size_t> Pick() override {
    pick_weights.clear();
    auto &threads = this->threads;

    auto eligible = [&](size_t i) {
      const bool is_free = threads[i].empty() || threads[i].back()->IsReturned();
      if (!this->AllowNewTasks() && is_free) return false;
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) return false;
      if (!is_free && !this->VerifyExistingTask(threads[i].back(), i)) {
        return false;
      }
      return true;
    };

    for (size_t i = 0; i < threads.size(); ++i) {
      if (!eligible(i)) continue;
      pick_weights.push_back(weights[i]);
    }

    if (pick_weights.empty()) [[unlikely]] {
      return std::nullopt;
    }

    auto thread_distribution =
        std::discrete_distribution<>(pick_weights.begin(), pick_weights.end());
    auto num = thread_distribution(this->rng);

    for (size_t i = 0; i < threads.size(); ++i) {
      if (!eligible(i)) continue;
      if (num == 0) return i;
      --num;
    }
    return std::nullopt;
  }

  std::optional<size_t> PickSchedule() override {
    pick_weights.clear();
    auto &threads = this->threads;

    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].size() ||
          threads[i][task_index]->IsBlocked() ||
          !this->VerifyExistingTask(threads[i][task_index], i)) {
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
          threads[i][task_index]->IsBlocked() ||
          !this->VerifyExistingTask(threads[i][task_index], i)) {
        continue;
      }
      if (num == 0) {
        return i;
      }
      num--;
    }
    return std::nullopt;
  }

  void SetCustomRound(CustomRound &custom_round) override {
    PickStrategy<TargetObj, Verifier>::SetCustomRound(custom_round);

    size_t custom_threads_count = custom_round.threads.size();
    weights.resize(custom_threads_count, 1);
    pick_weights.clear();
  }

 private:
  std::vector<int> weights;
  std::vector<int> pick_weights;
};
