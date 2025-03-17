
#pragma once
#include <algorithm>
#include <random>

#include "scheduler.h"

template <typename TargetObj, StrategyVerifier Verifier>
struct PickStrategy : Strategy<Verifier> {
  virtual size_t Pick() = 0;

  explicit PickStrategy(size_t threads_count,
                        std::vector<TaskBuilder> constructors)
      : next_task(0),
        threads_count(threads_count),
        constructors(std::move(constructors)),
        threads() {
    std::random_device dev;
    rng = std::mt19937(dev());
    distribution = std::uniform_int_distribution<std::mt19937::result_type>(
        0, this->constructors.size() - 1);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      threads.emplace_back();
    }
  }

  // If there aren't any non returned tasks and the amount of finished tasks
  // is equal to the max_tasks the finished task will be returned
  TaskWithMetaData Next() override {
    auto current_task = Pick();
    debug(stderr, "Picked thread: %zu\n", current_task);

    // it's the first task if the queue is empty
    if (threads[current_task].empty() ||
        threads[current_task].back()->IsReturned()) {
      // a task has finished or the queue is empty, so we add a new task
      std::shuffle(constructors.begin(), constructors.end(), rng);
      size_t verified_constructor = -1;
      for (size_t i = 0; i < constructors.size(); ++i) {
        TaskBuilder constructor = constructors.at(i);
        CreatedTaskMetaData next_task = {constructor.GetName(), true,
                                         current_task};
        if (this->sched_checker.Verify(next_task)) {
          verified_constructor = i;
          break;
        }
      }
      if (verified_constructor == -1) {
        assert(false && "Oops, possible deadlock or incorrect verifier\n");
      }
      threads[current_task].emplace_back(
          constructors[verified_constructor].Build(&state, current_task));
      TaskWithMetaData task{threads[current_task].back(), true, current_task};
      return task;
    }

    return {threads[current_task].back(), false, current_task};
  }

  void StartNextRound() override {
    TerminateTasks();
    for (auto& thread : threads) {
      // We don't have to keep references alive
      while (thread.size() > 0) {
        thread.pop_back();
      }
    }

    // Reinitial target as we start from the beginning.
    state.Reset();
  }

  ~PickStrategy() { TerminateTasks(); }

 protected:
  // Terminates all running tasks.
  // We do it in a dangerous way: in random order.
  // Actually, we assume obstruction free here.
  // TODO: for non obstruction-free we need to take into account dependencies.
  template<ManualReleased T = TargetObj>
  void TerminateTasks() {
    state.Release();
    for (size_t i = 0; i < threads.size(); ++i) {
      if (!threads[i].empty()) {
        threads[i].back()->Terminate();
      }
    }
    state.Reset();
    this->sched_checker.Reset();
  }

  template<typename T = TargetObj>
  void TerminateTasks() {
    for (size_t i = 0; i < threads.size(); ++i) {
      if (!threads[i].empty()) {
        threads[i].back()->Terminate();
      }
    }
    state.Reset();
    this->sched_checker.Reset();
  }

  TargetObj state{};
  size_t next_task = 0;
  size_t threads_count;
  std::vector<TaskBuilder> constructors;
  // RoundRobinStrategy struct is the owner of all tasks, and all
  // references can't be invalidated before the end of the round,
  // so we have to contains all tasks in queues(queue doesn't invalidate the
  // references)
  std::vector<StableVector<Task>> threads;
  std::uniform_int_distribution<std::mt19937::result_type> distribution;
  std::mt19937 rng;
};
