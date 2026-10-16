#pragma once
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
  SharedStrategyStatePtr shared_state =
      MapStrategySharedState<BaseStrategySharedState>();

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

  BaseStrategySharedState& GetSharedState() override { return *shared_state; }

  std::optional<size_t> Pick() override {
    pick_weights.clear();
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
      if (!eligible(i)) continue;
      pick_weights.push_back(weights[i]);
    }

    if (pick_weights.empty()) [[unlikely]] {
      return std::nullopt;
    }

    auto thread_distribution =
        std::discrete_distribution<>(pick_weights.begin(), pick_weights.end());
    auto num = thread_distribution(this->GetSharedState().rng);

    for (size_t i = 0; i < threads.size(); ++i) {
      if (!eligible(i)) continue;
      if (num == 0) return i;
      --num;
    }
    return std::nullopt;
  }

  std::optional<size_t> PickSchedule() override {
    weights.resize(this->threads.size(), 1);
    pick_weights.clear();
    auto &threads = this->threads;

    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].size() || !this->CanThreadContinue(i) ||
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
    auto num = thread_distribution(this->GetSharedState().rng);
    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      if (task_index == threads[i].size() || !this->CanThreadContinue(i) ||
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

  void SetCustomRound(const CustomRound &custom_round) override {
    PickStrategy<TargetObj, Verifier>::SetCustomRound(custom_round);

    size_t custom_threads_count = custom_round.threads.size();
    weights.resize(custom_threads_count, 1);
    pick_weights.clear();
  }

  void UpdateSimulatorState(size_t thread_id) override {
    BaseStrategyWithThreads<TargetObj, Verifier>::UpdateSimulatorState(thread_id);
    weights.resize(this->threads.size(), 1);
  }

 private:
  std::vector<int> weights;
  std::vector<int> pick_weights;
};

}  // namespace ltest
