#pragma once

#include <cassert>
#include <random>

#include "scheduler.h"

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos277-pct.pdf
// K represents the maximal number of potential switches in the program
// Although it's impossible to predict the exact number of switches(since it's
// equivalent to the halt problem), k should be good approximation
template <typename TargetObj, StrategyVerifier Verifier>
struct PctStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  PctStrategy(size_t threads_count, std::vector<TaskBuilder> ctrs)
      : BaseStrategyWithThreads<TargetObj, Verifier>(threads_count, ctrs),
        current_depth(1),
        current_schedule_length(0) {
    debug(stderr, "%ld:", this->constructors.size());
    // We have information about potential number of resumes
    // but because of the implementation, it's only available in the task.
    // In fact, it doesn't depend on the task, it only depends on the
    // constructor
    size_t avg_k = 0;
    avg_k = avg_k / this->constructors.size();

    PrepareForDepth(current_depth, avg_k);
  }

  // If there aren't any non returned tasks and the amount of finished tasks
  // is equal to the max_tasks the finished task will be returned
  TaskWithMetaData Next() override {
    return this->NextVerifiedFor(NextThreadId());
  }

  size_t NextThreadId() override {
    auto& threads = this->threads;
    ssize_t max = std::numeric_limits<ssize_t>::min();
    ssize_t snd_max = std::numeric_limits<ssize_t>::min();
    size_t index_of_max = 0, index_of_snd_max = 0;
    // Have to ignore waiting threads, so can't do it faster than O(n)
    for (size_t i = 0; i < threads.size(); ++i) {
      // Ignore waiting tasks
      // debug(stderr, "prior: %d, number %d\n", priorities[i], i);
      if (!threads[i].empty() && threads[i].back()->IsBlocked()) {
        // debug(stderr, "blocked\n", priorities[i], i);
        // dual waiting if request finished, but follow up isn't
        // skip dual tasks that already have finished the request
        // section(follow-up will be executed in another task, so we can't
        // resume)
        continue;
      }

      if (max <= priorities[i]) {
        index_of_snd_max = index_of_max;
        snd_max = max;
        max = priorities[i];
        index_of_max = i;
      } else if (snd_max <= priorities[i]) {
        snd_max = priorities[i];
        index_of_snd_max = i;
      }
    }

    // TODO: Choose wiser constant
    if (count_chosen_same == 100 && index_of_max == last_chosen && 
        snd_max != std::numeric_limits<ssize_t>::min()) {
      priorities[index_of_max] = snd_max - 1;
      index_of_max = index_of_snd_max;
    }

    if (index_of_max == last_chosen) {
      ++count_chosen_same;
    } else {
      count_chosen_same = 1;
    }

    assert(max != std::numeric_limits<ssize_t>::min() &&
           "all threads are empty or blocked");

    // Check whether the priority change is required
    current_schedule_length++;
    for (size_t i = 0; i < priority_change_points.size(); ++i) {
      if (current_schedule_length == priority_change_points[i]) {
        priorities[index_of_max] = current_depth - i;
      }
    }

    // debug(stderr, "Chosen thread: %d, cnt_count: %d\n", index_of_max,
    //       count_chosen_same);
    last_chosen = index_of_max;
    return index_of_max;
  }

  // NOTE: `Next` version use heuristics for livelock avoiding, but not there
  // refactor later to avoid copy-paste
  TaskWithMetaData NextSchedule() override {
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
    // Check whether the priority change is required
    current_schedule_length++;
    for (size_t i = 0; i < priority_change_points.size(); ++i) {
      if (current_schedule_length == priority_change_points[i]) {
        priorities[index_of_max] = current_depth - i;
      }
    }

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

    // current_depth have been increased
    size_t new_k = std::reduce(k_statistics.begin(), k_statistics.end()) /
                   k_statistics.size();
    log() << "k: " << new_k << "\n";
    PrepareForDepth(current_depth, new_k);
  }

  void PrepareForDepth(size_t depth, size_t k) {
    // Generates priorities
    priorities = std::vector<ssize_t>(this->threads_count);
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
  std::vector<ssize_t> priorities;
  std::vector<size_t> priority_change_points;
  std::mt19937 rng;
};
