#pragma once

#include <cassert>
#include <limits>
#include <optional>
#include <random>

#include "scheduler.h"

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos277-pct.pdf
// K represents the maximal number of potential switches in the program
// Although it's impossible to predict the exact number of switches(since it's
// equivalent to the halt problem), k should be good approximation
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct PctStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  PctStrategy(size_t threads_count, std::vector<TaskBuilder> ctrs)
      : BaseStrategyWithThreads<TargetObj, Verifier>(threads_count, ctrs),
        current_depth(1),
        current_schedule_length(0) {
    PrepareForDepth(current_depth, 1);
  }

  std::optional<size_t> NextThreadId() override {
    auto& threads = this->threads;
    size_t max = std::numeric_limits<size_t>::min();
    size_t index_of_max = 0;
    // Have to ignore waiting threads, so can't do it faster than O(n)
    for (size_t i = 0; i < threads.size(); ++i) {
      // Ignore waiting tasks
      // debug(stderr, "prior: %d, number %d\n", priorities[i], i);
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) {
        debug(stderr, "blocked on %p val %d\n",
              threads[i].back()->GetBlockState().addr,
              threads[i].back()->GetBlockState().value);
        // dual waiting if request finished, but follow up isn't
        // skip dual tasks that already have finished the request
        // section(follow-up will be executed in another task, so we can't
        // resume)
        continue;
      }

      if (max <= priorities[i]) {
        max = priorities[i];
        index_of_max = i;
      }
    }

    if (round_robin_stage > 0) [[unlikely]] {
      for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
        auto i = (++last_chosen) % threads.size();
        if (!threads[i].empty() && threads[i].back()->IsBlocked()) {
          continue;
        }
        index_of_max = i;
        max = priorities[i];
        break;
      }
      // debug(stderr, "round robin choose: %d\n", index_of_max);
      if (round_robin_start == index_of_max) {
        --round_robin_stage;
      }
    }

    // TODO: Choose wiser constant
    if (count_chosen_same == 1000 && index_of_max == last_chosen) [[unlikely]] {
      round_robin_stage = 5;
      round_robin_start = index_of_max;
    }

    if (index_of_max == last_chosen) {
      ++count_chosen_same;
    } else {
      count_chosen_same = 1;
    }

    if (max == std::numeric_limits<size_t>::min()) [[unlikely]] {
      return std::nullopt;
    }

    // Check whether the priority change is required
    current_schedule_length++;
    for (size_t i = 0; i < priority_change_points.size(); ++i) {
      if (current_schedule_length == priority_change_points[i]) {
        priorities[index_of_max] = current_depth - i;
      }
    }

    // debug(stderr, "Chosen thread: %d, cnt_count: %d\n", index_of_max,
    // count_chosen_same);
    last_chosen = index_of_max;
    return index_of_max;
  }

  // NOTE: `Next` version use heuristics for livelock avoiding, but not there
  // refactor later to avoid copy-paste
  std::optional<TaskWithMetaData> NextSchedule() override {
    auto& round_schedule = this->round_schedule;
    auto& threads = this->threads;
    size_t max = std::numeric_limits<size_t>::min();
    size_t index_of_max = 0;
    // Have to ignore waiting threads, so can't do it faster than O(n)
    for (size_t i = 0; i < threads.size(); ++i) {
      int task_index = this->GetNextTaskInThread(i);
      // Ignore waiting tasks
      if (task_index == threads[i].size() ||
          threads[i][task_index]->IsBlocked()) {
        // dual waiting if request finished, but follow up isn't
        // skip dual tasks that already have finished the request
        // section(follow-up will be executed in another task, so we can't
        // resume)
        continue;
      }

      if (max <= priorities[i]) {
        max = priorities[i];
        index_of_max = i;
      }
    }

    if (round_robin_stage > 0) [[unlikely]] {
      for (size_t attempt = 0; attempt < threads.size(); ++attempt) {
        auto i = (++last_chosen) % threads.size();
        int task_index = this->GetNextTaskInThread(i);
        if (task_index == threads[i].size() ||
            threads[i][task_index]->IsBlocked()) {
          continue;
        }
        index_of_max = i;
        max = priorities[i];
        break;
      }
      // debug(stderr, "round robin choose: %d\n", index_of_max);
      if (round_robin_start == index_of_max) {
        --round_robin_stage;
      }
    }

    // TODO: Choose wiser constant
    if (count_chosen_same == 1000 && index_of_max == last_chosen) [[unlikely]] {
      round_robin_stage = 5;
      round_robin_start = index_of_max;
    }

    if (index_of_max == last_chosen) {
      ++count_chosen_same;
    } else {
      count_chosen_same = 1;
    }

    if (max == std::numeric_limits<size_t>::min()) {
      return std::nullopt;
    }

    last_chosen = index_of_max;
    // Picked thread is `index_of_max`
    int next_task_index = this->GetNextTaskInThread(index_of_max);
    bool is_new = round_schedule[index_of_max] != next_task_index;
    round_schedule[index_of_max] = next_task_index;
    return TaskWithMetaData{threads[index_of_max][next_task_index], is_new,
                            index_of_max};
  }

  void StartNextRound() override {
    this->new_task_id = 0;
    //    log() << "depth: " << current_depth << "\n";
    // Reconstruct target as we start from the beginning.
    this->TerminateTasks();
    for (auto& thread : this->threads) {
      // We don't have to keep references alive
      while (thread.size() > 0) {
        thread.pop_back();
      }
      thread = StableVector<Task>();
    }
    // this->state.Reset();

    UpdateStatistics();
  }

  void ResetCurrentRound() override {
    BaseStrategyWithThreads<TargetObj, Verifier>::ResetCurrentRound();
    UpdateStatistics();
  }

  ~PctStrategy() { this->TerminateTasks(); }

 private:
  void UpdateStatistics() {
    // Update statistics
    current_depth++;
    if (current_depth >= 50) {
      current_depth = 50;
    }
    k_statistics.push_back(current_schedule_length);
    current_schedule_length = 0;
    count_chosen_same = 0;
    round_robin_stage = 0;

    // current_depth have been increased
    size_t new_k = std::reduce(k_statistics.begin(), k_statistics.end()) /
                   k_statistics.size();
    // log() << "k: " << new_k << "\n";
    PrepareForDepth(current_depth, new_k);
  }

  void PrepareForDepth(size_t depth, size_t k) {
    // Generates priorities
    priorities = std::vector<int>(this->threads_count);
    for (size_t i = 0; i < priorities.size(); ++i) {
      priorities[i] = current_depth + i;
    }
    std::shuffle(priorities.begin(), priorities.end(), rng);

    // Generates priority_change_points
    auto k_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(1, k);
    priority_change_points = std::vector<size_t>(depth - 1);
    for (size_t i = 0; i < depth - 1; ++i) {
      priority_change_points[i] = k_distribution(rng);
    }
  }

  std::vector<size_t> k_statistics;
  size_t current_depth;
  size_t current_schedule_length;
  // NOTE(kmitkin): added for livelock avoiding in spinlocks (read more in
  // original article)
  size_t count_chosen_same;
  size_t last_chosen;
  size_t round_robin_start;
  size_t round_robin_stage{0};
  std::vector<int> priorities;
  std::vector<size_t> priority_change_points;
  std::mt19937 rng;
};
