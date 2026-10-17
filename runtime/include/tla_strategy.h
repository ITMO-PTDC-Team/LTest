#pragma once

#include <cstdlib>
#include <cstring>

#include "scheduler.h"

namespace ltest {

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct TLAStrategy : public BaseStrategyWithThreads<TargetObj, Verifier> {
  using TargetFactory =
      typename BaseStrategyWithThreads<TargetObj, Verifier>::TargetFactory;
  TLAStrategy(size_t threads_count, std::vector<TaskBuilder> ctrs,
              TargetFactory target_factory)
      : BaseStrategyWithThreads<TargetObj, Verifier>(
            threads_count, std::move(ctrs), std::move(target_factory)) {}
  // use of this method doesn't make any sense, use Next() directly
  std::optional<size_t> NextThreadId() override {
    throw std::runtime_error("use Next() directly for TLA scheduler");
  }
  StrategyNextResult NextSchedule() override {
    if (auto task = this->GetReplayTask()) return *task;

    std::vector<size_t> choices;
    for (size_t tid = 0; tid < this->threads.size(); ++tid) {
      int index = this->GetNextTaskInThread(tid);
      if (index < static_cast<int>(this->threads[tid].size()) &&
          this->CanThreadContinue(tid) &&
          !this->threads[tid][index]->IsBlocked())
        choices.push_back(tid);
    }
    if (choices.empty()) return DEADLOCK;

    auto tid = choices[std::uniform_int_distribution<size_t>(
        0, choices.size() - 1)(this->GetSharedState().rng)];
    int index = this->GetNextTaskInThread(tid);
    this->round_schedule[tid] = index;
    return TaskWithMetaData{this->threads[tid][index], tid};
  }

  bool IsExhausted() override { return GetSharedState().is_exhausted; }

  StrategyNextResult Next() override {
    if (!exploring) return NextSchedule();
    auto& state = GetSharedState();
    if (frame_counter < state.frames_size) {
      auto frame = ReadFrame(frame_counter);
      frame_counter++;
      auto& tasks = this->threads[frame.thread_id];
      [[maybe_unused]] bool os_status = this->CanThreadContinue(frame.thread_id);
      assert(os_status && "couldn't verify os task in replay phase");

      if (frame.is_new) {
        tasks.emplace_back(this->BuildTask(frame.constr_id, frame.thread_id,
                                           this->new_task_id++));
        this->OnVerifierTaskStart(tasks.back(), frame.thread_id);
      }
      assert(frame.task_index < tasks.size() &&
             "couldn't find task in replay phase");
      bool verify_status = this->sched_checker.Verify(
          std::string{tasks[frame.task_index]->GetName()}, frame.thread_id,
          frame.is_new);
      assert(verify_status && "couldn't verify task in replay phase");
      return TaskWithMetaData{tasks[frame.task_index], frame.thread_id};
    }
    for (size_t i = start_thread_id; i < this->threads.size(); ++i) {
      auto& tasks = this->threads[i];
      if (!this->CanThreadContinue(i)) {
        continue;
      }
      if (!tasks.empty() && !tasks.back()->IsReturned()) {
        if (tasks.back()->IsBlocked()) {
          continue;
        }
        if (!this->sched_checker.Verify(std::string{tasks.back()->GetName()}, i,
                                        false)) {
          continue;
        }
        // Task exists.
        AppendFrame({i, 0, tasks.size() - 1, false});
        start_constr_id = 0;
        start_thread_id = 0;
        frame_counter++;
        return TaskWithMetaData{tasks.back(), i};
      }
      if (!this->AllowNewTasks() || !this->NewTaskAreEnabled(i)) {
        continue;
      }
      for (size_t constr_id = start_constr_id;
           constr_id < this->constructors.size(); ++constr_id) {
        auto cons = this->constructors[constr_id];
        if (!this->sched_checker.Verify(cons.GetName(), i, true)) {
          continue;
        }
        const size_t task_index = tasks.size();
        tasks.emplace_back(this->BuildTask(constr_id, i, this->new_task_id++));
        this->OnVerifierTaskStart(tasks.back(), i);
        AppendFrame({i, constr_id, task_index, true});
        start_constr_id = 0;
        start_thread_id = 0;
        frame_counter++;
        return TaskWithMetaData{tasks.back(), i};
      }
      start_thread_id = i + 1;
      start_constr_id = 0;
    }
    if (!state.frames_size) {
      state.is_exhausted = true;
    }
    return NEED_REPLAY;
  }
  void StartNextRound(const CustomRound* custom_round = nullptr) override {
    exploring =
        !custom_round && attempt_session->request.kind == AttemptKind::Generate;
    auto& state = GetSharedState();
    if (exploring && !state.seed) state.seed = state.rng();
    // Keep the input seed fixed while traversing this execution tree.
    if (!custom_round && state.seed) {
      std::srand(*state.seed);
      if (wmm::wmm_enabled)
        wmm::ExecutionGraph::GetInstance().Seed(*state.seed);
    }
    BaseStrategyWithThreads<TargetObj, Verifier>::StartNextRound(custom_round);
    frame_counter = 0;
    if (!exploring) return;
    if (!state.frames_size) return;
    frames.Map(state.frames_size * sizeof(Frame));
    auto frame = ReadFrame(--state.frames_size);
    if (!frame.is_new) {
      start_thread_id = frame.thread_id + 1;
      start_constr_id = 0;
    } else if (frame.constr_id + 1 == this->constructors.size()) {
      start_thread_id = frame.thread_id + 1;
      start_constr_id = 0;
    } else {
      start_thread_id = frame.thread_id;
      start_constr_id = frame.constr_id + 1;
    }
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
    size_t task_index{};
    // Is true if the task was created at this step.
    bool is_new{};
  };

  Frame ReadFrame(size_t index) const {
    Frame frame;
    const auto* data = static_cast<const std::byte*>(frames.mapping);
    std::memcpy(&frame, data + index * sizeof(Frame), sizeof(Frame));
    return frame;
  }

  void AppendFrame(const Frame& frame) {
    auto& count = GetSharedState().frames_size;
    frames.Reserve((count + 1) * sizeof(Frame));
    std::memcpy(static_cast<std::byte*>(frames.mapping) + count * sizeof(Frame),
                &frame, sizeof(Frame));
    ++count;
  }

  struct SharedState : StrategySharedState<SharedState> {
    std::optional<unsigned> seed;
    bool is_exhausted = false;
    size_t frames_size = 0;
  };

 public:
  SharedState& GetSharedState() override { return *shared_state; }

 private:
  MappedStrategyState<SharedState> shared_state;
  // Fixed fields and DFS frames use the same buffer. Its mappings stay local.
  Buffer& frames = shared_state.buffer;
  size_t frame_counter = 0;
  size_t start_thread_id = 0;
  size_t start_constr_id = 0;
  bool exploring = false;
};

}  // namespace ltest
