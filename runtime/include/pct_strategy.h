#pragma once

#include <cassert>
#include <random>

#include "scheduler.h"

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos277-pct.pdf
// K represents the maximal number of potential switches in the program
// Although it's impossible to predict the exact number of switches(since it's
// equivalent to the halt problem), k should be good approximation
template <typename TargetObj, StrategyVerifier Verifier>
struct PctStrategy : Strategy<Verifier> {
  // forbid_all_same indicates whether it is allowed to have all same tasks(same
  // methods) in any moment of execution. Useful for blocking structures, for
  // instance, you don't want to run mutex.lock in each thread
  explicit PctStrategy(size_t threads_count,
                       const std::vector<TaskBuilder>& constructors,
                       bool forbid_all_same)
      : threads_count(threads_count),
        current_depth(1),
        current_schedule_length(0),
        constructors(constructors),
        threads(),
        forbid_all_same(forbid_all_same) {
    std::random_device dev;
    rng = std::mt19937(dev());
    constructors_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(
            0, constructors.size() - 1);

    // We have information about potential number of resumes
    // but because of the implementation, it's only available in the task.
    // In fact, it doesn't depend on the task, it only depends on the
    // constructor
    size_t avg_k = 0;

    avg_k = avg_k / constructors.size();

    PrepareForDepth(current_depth, avg_k);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      threads.emplace_back();
    }
  }

  // If there aren't any non returned tasks and the amount of finished tasks
  // is equal to the max_tasks the finished task will be returned
  TaskWithMetaData Next() override {
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
      // a task has finished or the queue is empty, so we add a new task
      std::shuffle(constructors.begin(), constructors.end(), rng);
      size_t verified_constructor = -1;
      for (size_t i = 0; i < constructors.size(); ++i) {
        TaskBuilder constructor = constructors.at(i);
        CreatedTaskMetaData next_task = {constructor.GetName(), true,
                                         index_of_max};
        if (this->sched_checker.Verify(next_task)) {
          verified_constructor = i;
          break;
        }
      }
      if (verified_constructor == -1) {
        assert(false && "Oops, possible deadlock or incorrect verifier\n");
      }
      auto constructor = constructors.at(verified_constructor);
      if (forbid_all_same) {
        auto names = CountNames(index_of_max);
        // TODO: выглядит непонятно и так себе
        while (true) {
          auto name = constructor.GetName();
          names.insert(name);
          CreatedTaskMetaData task = {name, true, index_of_max};
          if (names.size() == 1 || !this->sched_checker.Verify(task)) {
            constructor = constructors.at(constructors_distribution(rng));
          } else {
            break;
          }
        }
      }

      threads[index_of_max].emplace_back(
          constructor.Build(&state, index_of_max));
      return {threads[index_of_max].back(), true, index_of_max};
    }

    return {threads[index_of_max].back(), false, index_of_max};
  }

  void StartNextRound() override {
    // Reconstruct target as we start from the beginning.
    TerminateTasks();
    for (auto& thread : threads) {
      // We don't have to keep references alive
      while (thread.size() > 0) {
        thread.pop_back();
      }
    }

    state.Reset();
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
    log() << "k: " << new_k << "\n";
    PrepareForDepth(current_depth, new_k);

    for (auto& thread : threads) {
      thread = StableVector<Task>();
    }
  }

  ~PctStrategy() { TerminateTasks(); }

 private:
  std::unordered_set<std::string> CountNames(size_t except_thread) {
    std::unordered_set<std::string> names;

    for (size_t i = 0; i < threads.size(); ++i) {
      auto& thread = threads[i];
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

  void TerminateTasks() {
    state.Reset();
    for (auto& thread : threads) {
      if (!thread.empty()) {
        thread.back()->Terminate();
      }
    }
    this->sched_checker.Reset();
  }

  TargetObj state{};
  std::vector<TaskBuilder> constructors;
  std::vector<size_t> k_statistics;
  size_t threads_count;
  size_t current_depth;
  size_t current_schedule_length;
  std::vector<size_t> priorities;
  std::vector<size_t> priority_change_points;
  // RoundRobinStrategy struct is the owner of all tasks, and all
  // references can't be invalidated before the end of the round,
  // so we have to contains all tasks in queues(queue doesn't invalidate the
  // references)
  std::vector<StableVector<Task>> threads;
  bool forbid_all_same;
  std::uniform_int_distribution<std::mt19937::result_type>
      constructors_distribution;
  std::mt19937 rng;
};
