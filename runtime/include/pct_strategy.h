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
  // forbid_all_same indicates whether it is allowed to have all same tasks(same
  // methods) in any moment of execution. Useful for blocking structures, for
  // instance, you don't want to run mutex.lock in each thread
  explicit PctStrategy(size_t threads_count,
                       std::vector<TaskBuilder> constructors,
                       bool forbid_all_same)
      : threads_count(threads_count),
        current_depth(1),
        current_schedule_length(0),
        forbid_all_same(forbid_all_same) {
    this->constructors = std::move(constructors);
    this->round_schedule.resize(threads_count, -1);

    std::random_device dev;
    rng = std::mt19937(dev());
    this->constructors_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(
            0, this->constructors.size() - 1);

    // We have information about potential number of resumes
    // but because of the implementation, it's only available in the task.
    // In fact, it doesn't depend on the task, it only depends on the
    // constructor
    size_t avg_k = 0;
    avg_k = avg_k / this->constructors.size();

    PrepareForDepth(current_depth, avg_k);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      this->threads.emplace_back();
    }
  }

  // If there aren't any non returned tasks and the amount of finished tasks
  // is equal to the max_tasks the finished task will be returned
  TaskWithMetaData Next() override {
    auto& threads = this->threads;
    size_t max = std::numeric_limits<size_t>::min();
    size_t index_of_max = 0;
    // Have to ignore waiting threads, so can't do it faster than O(n)
    for (size_t i = 0; i < threads.size(); ++i) {
      // Ignore waiting tasks
      if (!threads[i].empty() &&
          (threads[i].back()->IsParked() || threads[i].back()->IsBlocked())) {
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

    assert((max != std::numeric_limits<size_t>::min() &&
            "all threads are empty or parked"));

    // Check whether the priority change is required
    current_schedule_length++;
    for (size_t i = 0; i < priority_change_points.size(); ++i) {
      if (current_schedule_length == priority_change_points[i]) {
        priorities[index_of_max] = current_depth - i;
      }
    }

    if (threads[index_of_max].empty() ||
        threads[index_of_max].back()->IsReturned()) {
      auto constructor =
          this->constructors.at(this->constructors_distribution(rng));
      if (forbid_all_same) {
        auto names = CountNames(index_of_max);
        // TODO: выглядит непонятно и так себе
        while (true) {
          auto name = constructor.GetName();
          names.insert(name);
          CreatedTaskMetaData task = {name, true, index_of_max};
          if (names.size() == 1 || !this->sched_checker.Verify(task)) {
            constructor =
                this->constructors.at(this->constructors_distribution(rng));
          } else {
            break;
          }
        }
      }

      threads[index_of_max].emplace_back(
          constructor.Build(&this->state, index_of_max, this->new_task_id++));
      return {threads[index_of_max].back(), true, index_of_max};
    }

    return {threads[index_of_max].back(), false, index_of_max};
  }

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
          threads[i][task_index]->IsParked()) {
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
    //this->state.Reset();

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

    // current_depth have been increased
    size_t new_k = std::reduce(k_statistics.begin(), k_statistics.end()) /
                   k_statistics.size();
    // log() << "k: " << new_k << "\n";
    PrepareForDepth(current_depth, new_k);
  }

  std::unordered_set<std::string> CountNames(size_t except_thread) {
    std::unordered_set<std::string> names;

    for (size_t i = 0; i < this->threads.size(); ++i) {
      auto& thread = this->threads[i];
      if (thread.empty() || i == except_thread) {
        continue;
      }

      auto& task = thread.back();
      names.insert(std::string{task->GetName()});
    }

    return names;
  }

  void PrepareForDepth(size_t depth, size_t k) {
    // Generates priorities
    priorities = std::vector<size_t>(threads_count);
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
  size_t threads_count;
  size_t current_depth;
  size_t current_schedule_length;
  std::vector<size_t> priorities;
  std::vector<size_t> priority_change_points;
  // Strategy struct is the owner of all tasks, and all
  // references can't be invalidated before the end of the round,
  // so we have to contains all tasks in queues(queue doesn't invalidate the
  // references)
  bool forbid_all_same;
  std::mt19937 rng;
};
