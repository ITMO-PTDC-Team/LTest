#pragma once

#include <cassert>
#include <limits>
#include <optional>
#include <random>

#include "scheduler.h"

namespace ltest {

struct PctSharedState : StrategySharedState<PctSharedState> {
  size_t completed_rounds = 0;
  long double total_length = 0;
};

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos277-pct.pdf
// K represents the maximal number of potential switches in the program
// Although it's impossible to predict the exact number of switches(since it's
// equivalent to the halt problem), k should be good approximation
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct PctStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
 private:
  MappedStrategyState<PctSharedState> shared_state;

 public:
  using Base = BaseStrategyWithThreads<TargetObj, Verifier>;
  using TargetFactory = typename Base::TargetFactory;

  PctStrategy(size_t threads_count, std::vector<TaskBuilder> ctrs,
              TargetFactory target_factory)
      : Base(threads_count, std::move(ctrs), std::move(target_factory)) {}

  PctSharedState& GetSharedState() override { return *shared_state; }

  void StartNextRound(const CustomRound* custom_round = nullptr) override {
    current_depth =
        std::min<size_t>(this->GetSharedState().completed_rounds + 1, 50);
    current_schedule_length = 0;
    Base::StartNextRound(custom_round);
    if (!custom_round ||
        attempt_session->request.kind != AttemptKind::Generate) {
      PrepareForDepth(current_depth, MeanLength());
    }
    if (attempt_session->request.kind == AttemptKind::Generate)
      ++this->GetSharedState().completed_rounds;
  }

  void AvoidLivelock(size_t& index, size_t& prior) {
    if (runnable_threads.empty()) return;
    const bool overdue = std::any_of(
        runnable_threads.begin(), runnable_threads.end(), [&](size_t i) {
          return i != index && missed_selections[i] >= kOverdueAfter;
        });
    auto& rng = this->GetSharedState().rng;
    if (overdue && std::bernoulli_distribution(kReshuffleProbability)(rng)) {
      // Shuffle runnable priorities in place; the resulting order persists.
      // These extra changes do not inherit PCT's original probability bound.
      for (size_t remaining = runnable_threads.size(); remaining > 1;
           --remaining) {
        const size_t other =
            std::uniform_int_distribution<size_t>(0, remaining - 1)(rng);
        std::swap(priorities[runnable_threads[remaining - 1]],
                  priorities[runnable_threads[other]]);
      }
      for (const size_t i : runnable_threads) {
        if (priorities[index] <= priorities[i]) index = i;
      }
      prior = priorities[index];
    }

    // Blocked time does not count. Only a selection clears a thread's wait.
    for (const size_t i : runnable_threads) {
      missed_selections[i] =
          i == index ? 0 : std::min(missed_selections[i] + 1, kOverdueAfter);
    }
  }

  std::optional<size_t> NextThreadId() override {
    while (priorities.size() < this->threads.size()) AddPriority();
    auto& threads = this->threads;
    runnable_threads.clear();
    size_t max = std::numeric_limits<size_t>::min();
    size_t index_of_max = 0;
    // Have to ignore waiting threads, so can't do it faster than O(n)
    for (size_t i = 0; i < threads.size(); ++i) {
      // ignore if forbid generate new task
      const bool is_free =
          threads[i].empty() || threads[i].back()->IsReturned();
      if (!this->AllowNewTasks() && is_free) {
        continue;
      }
      // Ignore waiting tasks
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) {
        debug(stderr, "blocked on %p val %ld\n",
              reinterpret_cast<void*>(threads[i].back()->GetBlockState().addr),
              threads[i].back()->GetBlockState().value);
        // dual waiting if request finished, but follow up isn't
        // skip dual tasks that already have finished the request
        // section(follow-up will be executed in another task, so we can't
        // resume)
        continue;
      }
      if (!this->CanThreadContinue(i) || !this->NewTaskAreEnabled(i)) {
        continue;
      }

      runnable_threads.push_back(i);
      if (max <= priorities[i]) {
        max = priorities[i];
        index_of_max = i;
      }
    }

    AvoidLivelock(index_of_max, max);

    if (max == std::numeric_limits<size_t>::min()) [[unlikely]] {
      return std::nullopt;
    }

    // Check whether the priority change is required
    current_schedule_length++;
    if (attempt_session->request.kind == AttemptKind::Generate)
      ++this->GetSharedState().total_length;
    for (size_t i = 0; i < priority_change_points.size(); ++i) {
      if (current_schedule_length == priority_change_points[i]) {
        priorities[index_of_max] = current_depth - i - 1;
      }
    }

    return index_of_max;
  }

  StrategyNextResult NextSchedule() override {
    if (auto task = this->GetReplayTask()) return *task;
    while (priorities.size() < this->threads.size()) AddPriority();
    auto& round_schedule = this->round_schedule;
    auto& threads = this->threads;
    runnable_threads.clear();
    size_t max = std::numeric_limits<size_t>::min();
    size_t index_of_max = 0;
    // Have to ignore waiting threads, so can't do it faster than O(n)
    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      // Ignore waiting tasks
      if (task_index == threads[i].size() || !this->CanThreadContinue(i) ||
          threads[i][task_index]->IsBlocked()) {
        // dual waiting if request finished, but follow up isn't
        // skip dual tasks that already have finished the request
        // section(follow-up will be executed in another task, so we can't
        // resume)
        continue;
      }

      runnable_threads.push_back(i);
      if (max <= priorities[i]) {
        max = priorities[i];
        index_of_max = i;
      }
    }

    AvoidLivelock(index_of_max, max);

    if (max == std::numeric_limits<size_t>::min()) {
      return DEADLOCK;
    }

    // Check whether the priority change is required
    current_schedule_length++;
    if (attempt_session->request.kind == AttemptKind::Generate)
      ++this->GetSharedState().total_length;
    for (size_t i = 0; i < priority_change_points.size(); ++i) {
      if (current_schedule_length == priority_change_points[i]) {
        priorities[index_of_max] = current_depth - i - 1;
      }
    }

    // Picked thread is `index_of_max`
    int next_task_index = this->GetNextTaskInThread(index_of_max);
    round_schedule[index_of_max] = next_task_index;
    return TaskWithMetaData{threads[index_of_max][next_task_index],
                            index_of_max};
  }

  void SetCustomRound(const CustomRound& custom_round) override {
    Base::SetCustomRound(custom_round);
    PrepareForDepth(current_depth, MeanLength());
  }

 private:
  size_t MeanLength() {
    const auto& state = this->GetSharedState();
    return state.completed_rounds
               ? std::max<size_t>(1,
                                  state.total_length / state.completed_rounds)
               : 1;
  }

  void PrepareForDepth(size_t depth, size_t k) {
    missed_selections.clear();
    // Because we have custom rounds, take the
    // threads count from the vector size and not
    // from the threads_count field, which shows the passed threads count
    // argument from the command string
    size_t threads_count = this->threads.size();

    priorities.clear();
    priorities.reserve(threads_count);
    while (priorities.size() < threads_count) {
      AddPriority();
    }

    // Generates priority_change_points
    auto k_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(1, k);
    priority_change_points = std::vector<size_t>(depth - 1);
    for (size_t i = 0; i < depth - 1; ++i) {
      priority_change_points[i] = k_distribution(this->GetSharedState().rng);
    }
  }

  void UpdateSimulatorState(size_t thread_id) override {
    Base::UpdateSimulatorState(thread_id);
    if (this->threads.size() > this->priorities.size()) {
      AddPriority();
    }
  }

  void AddPriority() {
    const size_t min_priority = current_depth;
    auto priority_distribution = std::uniform_int_distribution(
        min_priority, std::numeric_limits<size_t>::max());

    size_t priority;
    do {
      priority = priority_distribution(this->GetSharedState().rng);
    } while (std::find(priorities.begin(), priorities.end(), priority) !=
             priorities.end());
    priorities.push_back(priority);
    missed_selections.push_back(0);
  }

  size_t current_depth = 1;
  size_t current_schedule_length = 0;
  static constexpr size_t kOverdueAfter = 1000;
  static constexpr double kReshuffleProbability = 0.01;
  std::vector<size_t> missed_selections;
  std::vector<size_t> runnable_threads;
  std::vector<size_t> priorities;
  std::vector<size_t> priority_change_points;
};

}  // namespace ltest
