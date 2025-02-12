
#pragma once
#include <queue>
#include <random>

#include "scheduler.h"

template <typename TargetObj>
struct PickStrategy : public BaseStrategyWithThreads<TargetObj> {
  virtual size_t Pick() = 0;

  virtual size_t PickSchedule() = 0;

  explicit PickStrategy(size_t threads_count,
                        std::vector<TaskBuilder> constructors)
      : next_task(0),
        threads_count(threads_count) {
    this->constructors = std::move(constructors);
    Strategy::round_schedule.resize(threads_count, -1);

    std::random_device dev;
    rng = std::mt19937(dev());
    this->constructors_distribution = std::uniform_int_distribution<std::mt19937::result_type>(
        0, this->constructors.size() - 1);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      this->threads.emplace_back();
    }
  }

  // If there aren't any non returned tasks and the amount of finished tasks
  // is equal to the max_tasks the finished task will be returned
  std::tuple<Task&, bool, int> Next() override {
    auto& threads = this->threads;
    auto current_thread = Pick();

    // it's the first task if the queue is empty
    if (threads[current_thread].empty() ||
        threads[current_thread].back()->IsReturned()) {
      // a task has finished or the queue is empty, so we add a new task
      auto constructor = this->constructors.at(this->constructors_distribution(rng));
      threads[current_thread].emplace_back(
          constructor.Build(&this->state, current_thread, Strategy::new_task_id++));
      return {threads[current_thread].back(), true, current_thread};
    }

    return {threads[current_thread].back(), false, current_thread};
  }

  std::tuple<Task&, bool, int> NextSchedule() override {
    auto& round_schedule = Strategy::round_schedule;
    size_t current_thread = PickSchedule();
    int next_task_index = this->GetNextTaskInThread(current_thread);
    bool is_new = round_schedule[current_thread] != next_task_index;

    round_schedule[current_thread] = next_task_index;
    return { this->threads[current_thread][next_task_index], is_new, current_thread };
  }

  void StartNextRound() override {
    Strategy::new_task_id = 0;

    this->TerminateTasks();
    for (auto& thread : this->threads) {
      // We don't have to keep references alive
      while (thread.size() > 0) {
        thread.pop_back();
      }
    }

    // Reinitial target as we start from the beginning.
    this->state.Reset();
  }

  ~PickStrategy() {
    this->TerminateTasks();
  }

protected:
  size_t next_task = 0;
  size_t threads_count;
  std::mt19937 rng;
};
