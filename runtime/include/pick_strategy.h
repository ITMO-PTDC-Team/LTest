
#pragma once
#include <algorithm>
#include <random>

#include "scheduler.h"

template <typename TargetObj, StrategyVerifier Verifier>
struct PickStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  virtual size_t Pick() = 0;

  virtual size_t PickSchedule() = 0;

  explicit PickStrategy(size_t threads_count,
                        std::vector<TaskBuilder> constructors)
      : next_task(0), threads_count(threads_count) {
    this->constructors = std::move(constructors);
    this->round_schedule.resize(threads_count, -1);

    std::random_device dev;
    rng = std::mt19937(dev());
    this->constructors_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(
            0, this->constructors.size() - 1);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      this->threads.emplace_back();
    }
  }

  // If there aren't any non returned tasks and the amount of finished tasks
  // is equal to the max_tasks the finished task will be returned
  TaskWithMetaData Next() override {
    auto& threads = this->threads;
    auto current_thread = Pick();
    DEBUG(stderr, "Picked thread: %zu\n", current_thread);

    // it's the first task if the queue is empty
    if (threads[current_thread].Empty() ||
        threads[current_thread].Back()->IsReturned()) {
      // a task has finished or the queue is empty, so we add a new task
      std::shuffle(this->constructors.begin(), this->constructors.end(), rng);
      size_t verified_constructor = -1;
      for (size_t i = 0; i < this->constructors.size(); ++i) {
        TaskBuilder constructor = this->constructors.at(i);
        CreatedTaskMetaData next_task = {constructor.GetName(), true,
                                         current_thread};
        if (this->sched_checker.Verify(next_task)) {
          verified_constructor = i;
          break;
        }
      }
      if (verified_constructor == -1) {
        assert(false && "Oops, possible deadlock or incorrect verifier\n");
      }
      threads[current_thread].EmplaceBack(
          this->constructors[verified_constructor].Build(
              &this->state, current_thread, this->new_task_id++));
      TaskWithMetaData task{threads[current_thread].Back(), true,
                            current_thread};
      return task;
    }

    return {threads[current_thread].Back(), false, current_thread};
  }

  TaskWithMetaData NextSchedule() override {
    auto& round_schedule = this->round_schedule;
    size_t current_thread = PickSchedule();
    int next_task_index = this->GetNextTaskInThread(current_thread);
    bool is_new = round_schedule[current_thread] != next_task_index;

    round_schedule[current_thread] = next_task_index;
    return TaskWithMetaData{this->threads[current_thread][next_task_index],
                            is_new, current_thread};
  }

  void StartNextRound() override {
    this->new_task_id = 0;

    this->TerminateTasks();
    for (auto& thread : this->threads) {
      // We don't have to keep references alive
      while (thread.Size() > 0) {
        thread.PopBack();
      }
    }

    // Reinitial target as we start from the beginning.
    // this->state.Reset();
  }

  ~PickStrategy() { this->TerminateTasks(); }

 protected:
  size_t next_task = 0;
  size_t threads_count;
  std::mt19937 rng;
};
