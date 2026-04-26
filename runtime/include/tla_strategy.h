#pragma once
#include "scheduler.h"

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct TLAStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  TLAStrategy(size_t threads_count, std::vector<TaskBuilder> ctrs)
      : BaseStrategyWithThreads<TargetObj, Verifier>(threads_count, ctrs) {}
  // use of this two methods doesn't make any sense, use Next() directly
  std::optional<size_t> NextThreadId() override {
    throw std::runtime_error("use Next() directly for TLA scheduler");
  }
  StrategyNextResult NextSchedule() override {
    throw std::runtime_error("use Next() directly for TLA scheduler");
  }

  bool IsExhausted() override {
    return is_exhausted;
  }

  StrategyNextResult Next() override {
    if (frame_counter < frames.size()) {
      auto frame = frames[frame_counter];
      frame_counter++;
      auto& tasks = this->threads[frame.thread_id];
      if (frame.is_new) {
        tasks.emplace_back(this->constructors[frame.constr_id].Build(
            this->state.get(), frame.thread_id, this->new_task_id++));
      }
      bool verify_status = this->sched_checker.Verify(
          std::string{tasks.back()->GetName()}, frame.thread_id, false);
      assert(verify_status && "couldn't verify task in replay phase");
      return TaskWithMetaData{tasks.back(), frame.is_new, frame.thread_id};
    }
    for (size_t i = start_thread_id; i < this->threads.size(); ++i) {
      auto& tasks = this->threads[i];
      if (!tasks.empty() && !tasks.back()->IsReturned() && !at_task_creation) {
        if (tasks.back()->IsBlocked()) {
          continue;
        }
        if (!this->sched_checker.Verify(std::string{tasks.back()->GetName()}, i,
                                        false)) {
          continue;
        }
        // Task exists.
        frames.emplace_back(i, 0, false);
        start_constr_id = 0;
        start_thread_id = 0;
        at_task_creation = false;
        frame_counter++;
        return TaskWithMetaData{tasks.back(), false, i};
      }

      for (size_t constr_id = start_constr_id;
           constr_id < this->constructors.size(); ++constr_id) {
        auto cons = this->constructors[constr_id];
        if (!this->sched_checker.Verify(cons.GetName(), i, true)) {
          continue;
        }
        tasks.emplace_back(
            cons.Build(this->state.get(), i, this->new_task_id++));
        frames.emplace_back(i, constr_id, true);
        start_constr_id = 0;
        start_thread_id = 0;
        at_task_creation = false;
        frame_counter++;
        return TaskWithMetaData{tasks.back(), true, i};
      }
      at_task_creation = false;
      start_thread_id = i + 1;
      start_constr_id = 0;
    }
    if (frames.empty()) {
      is_exhausted = true;
      return EXHAUSTED_INTERLEAVINGS;
    }
    return NEED_REPLAY;
  }
  void StartNextRound() override {
    BaseStrategyWithThreads<TargetObj, Verifier>::StartNextRound();
    frame_counter =0;
    auto frame = frames.back();
    if (!frame.is_new) {
      at_task_creation = true;
      start_thread_id = frame.thread_id;
      start_constr_id = 0;
    }
    else if (start_constr_id + 1 == this->constructors.size()) {
      start_thread_id = frame.thread_id + 1;
      start_constr_id = 0;
      at_task_creation = false;
    }
    else {
      at_task_creation = true;
      start_thread_id = frame.thread_id;
      start_constr_id = frame.constr_id + 1;
    }
    frames.pop_back();
  }

 private:

  // TLAScheduler enumerates all possible executions with finished max_tasks.
  // In fact, it enumerates tables (c = continue, f = finished):
  //         *---------*---------*--------*
  //         |   T1    |   T2    |   T3   |
  //         *---------*---------*--------*
  // state0  | task_i  |         |        |
  // state1  |    c    |         |        |
  // state2  |         | task_j  |        |
  // ...     |         |    c    |        |
  //         |    f    |         |        |
  //                      .....
  // Frame struct describes one row of this table.
  struct Frame {
    // we could use here a pointer to Task* but it brings problem at recreation
    // and is (probably) a UB
    size_t thread_id{};
    size_t constr_id{};
    // Is true if the task was created at this step.
    bool is_new{};
  };
  size_t frame_counter = 0;
  bool at_task_creation = false;
  bool frame_added = false;
  size_t start_thread_id = 0;
  size_t start_constr_id = 0;
  StableVector<Frame> frames;
  bool is_exhausted = false;
};