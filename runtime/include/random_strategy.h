#pragma once
#include <cstdint>
#include <random>
#include <utility>
#include <vector>

#include "lib.h"
#include "pick_strategy.h"

namespace ltest {

// Allows a random thread to work.
// Randoms new task.
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct RandomStrategy : PickStrategy<TargetObj, Verifier> {
 private:
  MappedStrategyState<BaseStrategySharedState> shared_state;

 public:
  using TargetFactory =
      typename PickStrategy<TargetObj, Verifier>::TargetFactory;

  explicit RandomStrategy(size_t threads_count,
                          std::vector<TaskBuilder> constructors,
                          std::vector<int> weights,
                          TargetFactory target_factory)
      : PickStrategy<TargetObj, Verifier>{threads_count,
                                          std::move(constructors),
                                          std::move(target_factory)},
        weights{std::move(weights)} {}

  BaseStrategySharedState &GetSharedState() override { return *shared_state; }

  std::optional<size_t> Pick() override {
    weights.resize(this->threads.size(), 1);
    pick_threads.clear();
    auto &threads = this->threads;

    auto eligible = [&](size_t i) {
      const bool is_free = threads[i].empty() || threads[i].back()->IsReturned();
      if (!this->AllowNewTasks() && is_free) return false;
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) return false;
      if (!this->CanThreadContinue(i) || !this->NewTaskAreEnabled(i)) {
        return false;
      }
      return true;
    };

    for (size_t i = 0; i < threads.size(); ++i) {
      if (weights[i] == 0) continue;
      if (!eligible(i)) continue;
      pick_threads.push_back(i);
    }

    return PickWeightedThread();
  }

  std::optional<size_t> PickSchedule() override {
    weights.resize(this->threads.size(), 1);
    pick_threads.clear();
    auto &threads = this->threads;

    for (size_t i = 0; i < threads.size(); ++i) {
      if (weights[i] == 0) continue;
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].size() || !this->CanThreadContinue(i) ||
          threads[i][task_index]->IsBlocked()) {
        continue;
      }
      pick_threads.push_back(i);
    }

    return PickWeightedThread();
  }

 private:
  std::vector<int> weights;
  std::vector<size_t> pick_threads;

  std::optional<size_t> PickWeightedThread() {
    if (pick_threads.empty()) return std::nullopt;
    if (pick_threads.size() == 1) return pick_threads.front();

    std::uint64_t total = 0;
    for (size_t thread : pick_threads) total += weights[thread];
    auto ticket = std::uniform_int_distribution<std::uint64_t>(0, total - 1)(
        this->GetSharedState().rng);
    for (size_t thread : pick_threads) {
      const auto weight = static_cast<std::uint64_t>(weights[thread]);
      if (ticket < weight) return thread;
      ticket -= weight;
    }
    return std::nullopt;
  }

};

}  // namespace ltest
