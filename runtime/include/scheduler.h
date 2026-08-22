#pragma once
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <functional>
#include <iterator>
#include <limits>
#include <optional>
#include <random>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "custom_round.h"
#include "coro_ctx_guard.h"
#include "lib.h"
#include "lincheck.h"
#include "lincheck_dual.h"
#include "logger.h"
#include "os_simulator.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"
#include "stable_vector.h"
#include "wmm/wmm.h"
#include "workload_policy.h"

namespace ltest {

struct TaskWithMetaData {
  Task& task;
  bool is_new;
  size_t thread_id;
};

enum StrategyNextScheduleFailure {
  DEADLOCK,
  NEED_REPLAY,
  EXHAUSTED_INTERLEAVINGS
};

using StrategyNextResult =
    std::variant<StrategyNextScheduleFailure, TaskWithMetaData>;

inline Scheduler::FullHistory ConvFullHistWithThreadToFullHist(
    const FullHistoryWithThreads& full) {
  Scheduler::FullHistory res;
  for (auto& f : full) {
    if (auto t = std::get_if<std::reference_wrapper<Task>>(&f.second)) {
      res.push_back(*t);
    }
  }
  return res;
}

/// StrategyTaskVerifier is required for scheduling only allowed tasks
/// Some data structures doesn't allow us to schedule one tasks before another
/// e.g. Mutex -- we are not allowed to schedule unlock before lock call, it is
/// UB.
template <typename T>
concept StrategyTaskVerifier = requires(T a) {
  {
    a.Verify(std::declval<const std::string&>(), size_t(), bool())
  } -> std::same_as<bool>;
  { a.OnFinished(std::declval<Task&>(), size_t()) } -> std::same_as<void>;
  { a.Reset() } -> std::same_as<void>;
};

struct OnlyOneTaskPerThreadVerifier {
  bool Verify(const std::string&, size_t thread_id, bool is_new) {
    if (!is_new) {
      return true;
    }
    if (thread_id != 0 || has_started) {
      return false;
    }
    has_started = true;
    return true;
  }

  bool VerifyExisting(Task&, size_t) { return true; }

  void OnFinished(Task&, size_t) {
    // intentionally do nothing
  }

  void Reset() {
    std::cerr << "Reset called\n";
    has_started = false;
  }

  bool has_started = false;
};

namespace verifier_hooks {

template <class V>
void OnRoundStart(V& v, std::size_t threads) {
  if constexpr (requires { v.OnRoundStart(threads); }) {
    v.OnRoundStart(threads);
  }
}

template <class V>
void OnTaskStarted(V& v, const std::string& method, std::size_t thread_id,
                   int task_id) {
  if constexpr (requires { v.OnTaskStarted(method, thread_id, task_id); }) {
    v.OnTaskStarted(method, thread_id, task_id);
  }
}

template <class V>
bool VerifyStart(V& v, const std::string& method, std::size_t thread_id,
                 const StartContext& ctx) {
  if constexpr (requires { v.VerifyStart(method, thread_id, ctx); }) {
    return v.VerifyStart(method, thread_id, ctx);
  } else {
    return true;
  }
}

}  // namespace verifier_hooks

// Strategy is the general strategy interface which decides which task
// will be the next one it can be implemented by different strategies, such as:
// randomized/tla/fair
struct Strategy {
  virtual bool IsExhausted() = 0;
  virtual std::optional<size_t> NextThreadId() = 0;

  virtual StrategyNextResult Next() = 0;

  virtual void TerminateTasks() = 0;

  virtual void ResetExplorationState() = 0;

  // Returns the same data as `Next` method. However, it does not generate the
  // round by inserting new tasks in it, but schedules the threads according to
  // the strategy policy with previously generated and saved round (used for
  // round replaying functionality)
  virtual StrategyNextResult NextSchedule() = 0;

  // Returns { task, its thread id } by task id. (TODO: make it `const` method)
  // This is a pure lookup over the task set already generated for the round.
  // It does NOT answer whether executing this task is still semantically legal
  // in the current replay/reset state.
  virtual std::optional<std::tuple<Task&, int>> GetTask(int task_id) = 0;

  // Checks whether an already-existing task is still legal to execute in the
  // current replay/reset state.
  //
  // Why this is needed:
  // A task sequence generated in one execution may become semantically invalid
  // in another replay. Example: an old `unlock()` task may still exist in the
  // thread queue, but the preceding `lock()` did not complete normally in this
  // replay. In that case replay must not execute that `unlock()`.
  virtual bool VerifyExistingTask(Task& task, size_t thread_id) = 0;

  // TODO: abstract this method more (returning `vector<StableVector<...>>` is
  // not good)
  virtual const std::vector<StableVector<Task>>& GetTasks() const = 0;

  // Returns true if the task with the given id is marked as removed
  bool IsTaskRemoved(int task_id) const {
    return removed_tasks.contains(task_id);
  }

  // Marks or demarks task as removed
  void SetTaskRemoved(int task_id, bool is_removed) {
    if (is_removed)
      removed_tasks.insert(task_id);
    else
      removed_tasks.erase(task_id);
  }

  void SetAllowNewTasks(bool v) { allow_new_tasks_ = v; }
  bool AllowNewTasks() const { return allow_new_tasks_; }

  virtual ~Strategy() = default;

  // Removes all tasks to start a new round.
  // (Note: strategy should stop all tasks that already have been started)
  virtual void StartNextRound() = 0;

  // Resets the state of all created tasks in the strategy.
  virtual void ResetCurrentRound() = 0;

  // Sets custom round provided by the user for execution.
  // The round should be executed via `Scheduler::ExploreRound` method
  // instead of `Scheduler::GenerateAndRunRound`.
  virtual void SetCustomRound(CustomRound& custom_round) = 0;

  // Returns the number of non-removed tasks
  virtual int GetValidTasksCount() const = 0;

  // Returns the total number of tasks (including removed)
  virtual int GetTotalTasksCount() const = 0;

  // Returns the number of threads
  virtual int GetThreadsCount() const = 0;

  virtual void UpdateSimulatorState(size_t thread_id,
                                  Scheduler::SeqHistory& seq,
                                  FullHistoryWithThreads& full) = 0;
  // Called when the finished task must be reported to the verifier
  // (Strategy is a pure interface, the templated subclass
  // BaseStrategyWithThreads knows about the Verifier and will delegate to that)
  virtual void OnVerifierTaskFinish(Task& task, size_t thread_id) = 0;

  // Checks whether starting this already-created task is semantically legal
  // in the current replay/exploration state.
  virtual bool VerifyTaskStart(Task& task, size_t thread_id,
                               const StartContext& ctx) = 0;

  // Reports a semantic start of a task in replay/exploration mode.
  virtual void OnVerifierTaskStart(Task& task, size_t thread_id) = 0;

  // Appends a concrete method call to an existing generated round. The task is
  // not semantically started here; replay/exploration will start it normally.
  virtual std::optional<int> AppendTaskForReplay(std::string_view method_name,
                                                 size_t thread_id) = 0;

  // Removes the task appended by AppendTaskForReplay. This is intentionally
  // restricted to the thread tail so rollback cannot silently rewrite an
  // arbitrary generated round.
  virtual bool RemoveLastTaskForReplay(size_t thread_id, int task_id) = 0;

  virtual bool HasTaskBuilder(std::string_view method_name) const = 0;

  virtual std::vector<std::string> GetDeadlockProgressMethods(
      std::string_view wait_method) const = 0;

 protected:
  // For current round returns first task index in thread which is greater
  // than `round_schedule[thread]` or the same index if the task is not finished
  virtual int GetNextTaskInThread(int thread_index) const = 0;

  void ResetWmmGraph(int threads_count) {
    if (wmm::wmm_enabled) {
      wmm_graph.Reset(threads_count);
    }
  }

  // id of next generated task
  int new_task_id = 0;
  // stores task ids that are removed during the round minimization
  std::unordered_set<int> removed_tasks;
  // when generated round is explored this vector stores indexes of tasks
  // that will be invoked next in each thread
  std::vector<int> round_schedule;
  ltest::wmm::ExecutionGraph& wmm_graph =
      ltest::wmm::ExecutionGraph::GetInstance();
  bool allow_new_tasks_ = true;
};

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct BaseStrategyWithThreads : public Strategy, OSSimulator {
  using TargetFactory = std::function<std::unique_ptr<TargetObj>()>;

  BaseStrategyWithThreads(size_t threads_count,
                          std::vector<TaskBuilder> constructors,
                          TargetFactory target_factory, size_t seed = 0)
      : threads_count(threads_count),
        constructors(std::move(constructors)),
        target_factory(std::move(target_factory)) {
    // must be called before instantiating `TargetObj`
    ResetWmmGraph(this->threads_count);
    round_schedule.resize(threads_count, -1);
    ResetTargetState();

    constructors_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(
            0, constructors.size() - 1);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      threads.emplace_back();
    }

    if (seed == 0) {
      std::random_device dev;
      rng = std::mt19937(dev());
    } else {
      rng = std::mt19937(seed);
    }

    verifier_hooks::OnRoundStart(sched_checker, threads_count);
  }

  ~BaseStrategyWithThreads() override {
    TerminateRunningTasks();
    CoroCtxGuard guard;
    (void)state.release();
  }

  std::optional<std::tuple<Task&, int>> GetTask(int task_id) override {
    // TODO: can this be optimized?
    int thread_id = 0;
    for (auto& thread : threads) {
      size_t tasks = thread.size();

      for (size_t i = 0; i < tasks; ++i) {
        Task& task = thread[i];
        if (task->GetId() == task_id) {
          std::tuple<Task&, int> result = {task, thread_id};
          return result;
        }
      }

      thread_id++;
    }
    return std::nullopt;
  }

  const std::vector<StableVector<Task>>& GetTasks() const override {
    return threads;
  }

  bool HasTaskBuilder(std::string_view method_name) const override {
    return std::any_of(
        constructors.begin(), constructors.end(),
        [&](const TaskBuilder& b) { return b.GetName() == method_name; });
  }

  std::optional<int> AppendTaskForReplay(std::string_view method_name,
                                         size_t thread_id) override {
    if (thread_id >= threads.size()) {
      return std::nullopt;
    }

    auto constructor_it = std::find_if(
        constructors.begin(), constructors.end(),
        [&](const TaskBuilder& b) { return b.GetName() == method_name; });

    if (constructor_it == constructors.end()) {
      return std::nullopt;
    }

    Task task = constructor_it->Build(state.get(), thread_id, new_task_id++);
    const int task_id = task->GetId();
    threads[thread_id].emplace_back(std::move(task));
    return task_id;
  }

  bool RemoveLastTaskForReplay(size_t thread_id, int task_id) override {
    if (thread_id >= threads.size() || threads[thread_id].empty()) {
      return false;
    }

    auto& thread = threads[thread_id];
    if (thread.back()->GetId() != task_id) {
      return false;
    }

    thread.pop_back();
    SetTaskRemoved(task_id, false);
    return true;
  }

  std::vector<std::string> GetDeadlockProgressMethods(
      std::string_view wait_method) const override {
    if constexpr (requires(const Verifier& verifier,
                           const std::string& method) {
                    {
                      verifier.GetDeadlockProgressMethods(method)
                    } -> std::same_as<std::vector<std::string>>;
                  }) {
      return sched_checker.GetDeadlockProgressMethods(std::string(wait_method));
    } else {
      return {};
    }
  }

  void StartNextRound() override {
    this->SetAllowNewTasks(true);
    this->new_task_id = 0;
    this->TerminateRunningTasks();
    this->sched_checker.Reset();
    // Must precede target construction so atomics register in the new graph.
    this->ResetWmmGraph(this->threads.size());
    ResetOSState();
    this->ResetTargetState();
    // this could happen if we run custom scenarios
    // (which could have arbitrary number of threads)
    if (this->threads.size() != this->threads_count) {
      this->threads.clear();
      for (size_t i = 0; i < this->threads_count; ++i) {
        this->threads.emplace_back();
      }
    } else {
      // more optimal allocations-wise implementation
      for (auto& thread : this->threads) {
        // We don't have to keep references alive
        while (!thread.empty()) {
          thread.pop_back();
        }
      }
    }

    this->round_schedule.resize(this->threads_count, -1);
    ltest::verifier_hooks::OnRoundStart(this->sched_checker,
                                        this->threads_count);
  }

  void ResetCurrentRound() override {
    this->SetAllowNewTasks(true);
    std::fill(round_schedule.begin(), round_schedule.end(), -1);
    TerminateRunningTasks();

    sched_checker.Reset();
    ResetWmmGraph(threads.size());
    ResetOSState();
    ResetTargetState();

    // New round/replay starts from fresh target state, so verifier state
    // must also be reset.
    ltest::verifier_hooks::OnRoundStart(sched_checker, threads_count);

    for (auto& thread : threads) {
      size_t tasks_in_thread = thread.size();
      for (size_t i = 0; i < tasks_in_thread; ++i) {
        if (!IsTaskRemoved(thread[i]->GetId())) {
          thread[i] = thread[i]->Restart(state.get());
        }
      }
    }
  }

  void SetCustomRound(CustomRound& custom_round) override {
    size_t custom_threads_count = custom_round.threads.size();

    // custom round threads count might differ from the generated rounds
    this->threads.resize(custom_threads_count);
    this->round_schedule.resize(custom_threads_count, -1);
    this->sched_checker.Reset();
    ResetWmmGraph(custom_threads_count);
    this->ResetTargetState();

    for (size_t current_thread = 0; current_thread < custom_threads_count;
         ++current_thread) {
      auto& builders = custom_round.threads[current_thread];
      StableVector<Task> thread_tasks;
      for (auto& task_builder : builders) {
        auto task =
            task_builder.Build(state.get(), current_thread, new_task_id++);
        thread_tasks.emplace_back(task);
      }
      this->threads[current_thread] = std::move(thread_tasks);
    }
    ltest::verifier_hooks::OnRoundStart(this->sched_checker,
                                        custom_threads_count);
  }

  int GetValidTasksCount() const override {
    int non_removed_tasks = 0;
    for (auto& thread : threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        auto& task = thread[i];
        if (!IsTaskRemoved(task->GetId())) {
          non_removed_tasks++;
        }
      }
    }
    return non_removed_tasks;
  }

  int GetTotalTasksCount() const override {
    int total_tasks = 0;
    for (auto& thread : threads) {
      total_tasks += thread.size();
    }
    return total_tasks;
  }

  int GetThreadsCount() const override { return threads.size(); }

  void OnVerifierTaskFinish(Task& task, size_t thread_id) override {
    sched_checker.OnFinished(task, thread_id);
  }

  bool VerifyTaskStart(Task& task, size_t thread_id,
                       const ltest::StartContext& ctx) override {
    return ltest::verifier_hooks::VerifyStart(
               sched_checker, std::string(task->GetName()), thread_id, ctx) &&
           sched_checker.Verify(std::string(task->GetName()), thread_id, true);
  }

  void OnVerifierTaskStart(Task& task, size_t thread_id) override {
    ltest::verifier_hooks::OnTaskStarted(
        sched_checker, std::string(task->GetName()), thread_id, task->GetId());
  }

  // Re-check legality of an already-generated task in the current replay state.
  // This is used by ReplayRound(), where task ordering is fixed in advance and
  // later tasks of a thread may become invalid if earlier tasks did not
  // complete normally in this replay.
  bool VerifyExistingTask(Task& task, size_t thread_id) override {
    if constexpr (requires(Verifier& verifier, Task& existing_task,
                           size_t existing_thread_id) {
                    verifier.VerifyExisting(existing_task, existing_thread_id);
                  }) {
      return sched_checker.VerifyExisting(task, thread_id);
    } else {
      return sched_checker.Verify(std::string(task->GetName()), thread_id, true);
    }
  }

  StrategyNextResult Next() override {
    return NextVerifiedFor(NextThreadId());
  }

  StrategyNextResult NextVerifiedFor(
      std::optional<size_t> opt_thread_index) {
    if (!opt_thread_index.has_value()) {
      return DEADLOCK;
    }
    size_t thread_index = opt_thread_index.value();
    // it's the first task if the queue is empty
    bool is_new = threads[thread_index].empty() ||
                  threads[thread_index].back()->IsReturned();
    if (is_new) {
      // Build start context (Blocked-trigger).
      ltest::StartContext ctx{};
      ctx.threads = this->threads.size();

      for (size_t tid = 0; tid < this->threads.size(); ++tid) {
        bool is_free = this->threads[tid].empty() ||
                       this->threads[tid].back()->IsReturned();
        if (is_free) {
          ctx.free_threads++;
          continue;
        }

        // Active task exists and is not returned.
        auto& active = this->threads[tid].back();
        std::string name = std::string(active->GetName());

        // NEW: count in-flight operations by method name
        ctx.active_by_method[name]++;

        // Keep blocked too (might be useful later)
        if (active->IsBlocked()) {
          ctx.blocked_by_method[name]++;
        }
      }

      // Choose constructor subject to verifier policy.
      std::shuffle(this->constructors.begin(), this->constructors.end(), rng);
      size_t verified_constructor = static_cast<size_t>(-1);

      for (size_t i = 0; i < this->constructors.size(); ++i) {
        const TaskBuilder& constructor = this->constructors.at(i);

        if (verifier_hooks::VerifyStart(
                sched_checker, constructor.GetName(), thread_index, ctx) &&
            this->sched_checker.Verify(constructor.GetName(), thread_index, is_new)) {
          verified_constructor = i;
          break;
        }
      }

      if (verified_constructor == static_cast<size_t>(-1)) {
        return DEADLOCK;
      }

      const TaskBuilder& chosen = this->constructors[verified_constructor];
      const std::string& method_name = chosen.GetName();

      Task task =
          chosen.Build(this->state.get(), thread_index, this->new_task_id++);
      verifier_hooks::OnTaskStarted(sched_checker, method_name,
                                           thread_index, task->GetId());

      threads[thread_index].emplace_back(std::move(task));
    }

    return TaskWithMetaData{threads[thread_index].back(), is_new, thread_index};
  }

  // Terminates all running tasks.
  void TerminateTasks() override {
    TerminateRunningTasks();
    sched_checker.Reset();
    // must appear before state reset, so that constructors of atomics in
    // data structure under test register themselves in the new execution graph
    // TODO: for custom scenarios threads number might differ, check for places
    // where `threads.size()` cannot be used
    ResetWmmGraph(threads.size());
    ResetTargetState();
  }

  void TerminateRunningTasks() {
    for (auto& thread : threads) {
      for (size_t i = 0; i < thread.size(); i++) {
        if (!thread[i]->IsReturned()) {
          thread[i]->Terminate();
        }
        thread[i]->DiscardRoundResults();
      }
    }
  }

  // Lightweight reset strictly for ExploreRound loops.
  // Keeps task objects & scheduler bookkeeping intact, resets spec state &
  // block queues.
  void ResetExplorationState() override {
    TerminateRunningTasks();
    ResetOSState();
    std::fill(round_schedule.begin(), round_schedule.end(), -1);
    this->SetAllowNewTasks(true);
    sched_checker.Reset();
    ResetWmmGraph(threads.size());
    ResetTargetState();
    verifier_hooks::OnRoundStart(sched_checker, threads_count);

    for (auto& thread : threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        if (!IsTaskRemoved(thread[i]->GetId())) {
          thread[i] = thread[i]->Restart(this->state.get());
        }
      }
    }
  }

 protected:

  /// Returns task id in threads[thread_index] skipping removed tasks
  int GetNextTaskInThread(int thread_index) const override {
    auto& thread = threads[thread_index];
    int task_index = round_schedule[thread_index];

    while (task_index < static_cast<int>(thread.size()) &&
           (task_index == -1 || thread[task_index].get()->IsReturned() ||
            IsTaskRemoved(thread[task_index].get()->GetId()))) {
      task_index++;
    }

    return task_index;
  }

  void UpdateSimulatorState(size_t thread_id, Scheduler::SeqHistory& seq,
                          FullHistoryWithThreads& full) override {
    return this->UpdateOSState(thread_id, seq, full);
  }

  // it's a bad workaround, but i really doesn't know how to implement it better
  bool NewTaskAreEnabled(size_t thread_id) {
    if constexpr (std::is_same_v<Verifier, OnlyOneTaskPerThreadVerifier>) {
      bool is_new =
          threads[thread_id].empty() || threads[thread_id].back()->IsReturned();
      if (is_new && !threads[thread_id].empty()) {
        return false;
      }
    }
    return true;
  }

  Verifier sched_checker{};
  std::unique_ptr<TargetObj> state;
  // Strategy struct is the owner of all tasks, and all
  // references can't be invalidated before the end of the round,
  // so we have to contains all tasks in queues(queue doesn't invalidate the
  // references)
  size_t threads_count;  // number of threads specified in the command string
                         // (in case if custom rounds are run, their threads
                         // count might be different from this value which is
                         // used for generated rounds only)
  std::vector<TaskBuilder> constructors;
  TargetFactory target_factory;
  std::uniform_int_distribution<std::mt19937::result_type>
      constructors_distribution;
  std::mt19937 rng;

 private:
  void ResetTargetState() {
    CoroCtxGuard guard;
    TargetConstructionGuard target_construction_guard;
    // Target objects are per-round mocks; abandoning them keeps destructor
    // side effects out of exploration reset.
    (void)state.release();
    state = target_factory();
  }
};

}  // namespace ltest

#include "minimization.h"
#include "minimization_smart.h"

namespace ltest {

static inline std::pair<bool, bool> GetUnfinishedAndRunnable(
    const Strategy& strategy, const std::unordered_set<int>* allowed_ids) {
  bool has_unfinished = false;
  bool has_runnable = false;

  const auto& threads = strategy.GetTasks();
  for (int tid = 0; tid < static_cast<int>(threads.size()); ++tid) {
    const auto& thr = threads[tid];
    for (int i = 0; i < static_cast<int>(thr.size()); ++i) {
      const auto& t = thr[i];
      int id = t->GetId();

      // NEW: if allowed_ids provided, ignore tasks outside ordering
      if (allowed_ids && !allowed_ids->contains(id)) continue;

      if (strategy.IsTaskRemoved(id)) continue;
      if (t->IsReturned()) continue;

      has_unfinished = true;
      if (!t->IsBlocked()) {
        has_runnable = true;
        return {has_unfinished, has_runnable};
      }
    }
  }

  return {has_unfinished, has_runnable};
}

static StartContext BuildReplayStartContext(
    const Strategy& strategy, const std::unordered_set<int>& started_ids) {
  StartContext ctx{};
  const auto& threads = strategy.GetTasks();
  ctx.threads = threads.size();

  for (size_t tid = 0; tid < threads.size(); ++tid) {
    bool has_active_started_task = false;

    for (size_t i = 0; i < threads[tid].size(); ++i) {
      const auto& task = threads[tid][i];
      int id = task->GetId();

      // Only tasks that have already semantically started in this
      // replay/explore contribute to the context.
      if (!started_ids.contains(id)) {
        continue;
      }
      if (strategy.IsTaskRemoved(id)) {
        continue;
      }
      if (task->IsReturned()) {
        continue;
      }

      has_active_started_task = true;

      std::string name = std::string(task->GetName());
      ctx.active_by_method[name]++;

      if (task->IsBlocked()) {
        ctx.blocked_by_method[name]++;
      }
    }

    if (!has_active_started_task) {
      ctx.free_threads++;
    }
  }

  return ctx;
}

template <class Event>
static inline bool IsReportableDeadlockHistory(const std::vector<Event>& seq) {
  // An empty/no-op replay can mean that no legal operation was generated,
  // but it is not a user-visible deadlock.
  return RoundMinimizorT<Event>::HasAnyStartedOp(seq) &&
         MinBadTraits<Event>::HasPendingVisibleOp(seq);
}

static inline void AppendDualStartEvent(std::vector<DualHistoryEvent>& seq,
                                        Task& task, bool is_new,
                                        int thread_id) {
  if (!is_new) return;
  if (task->IsDual()) {
    seq.emplace_back(RequestInvoke(task, thread_id));
  } else {
    seq.emplace_back(Invoke(task, thread_id));
  }
}

struct DrainedDualEventRecord {
  std::uint64_t seqno;
  Task task;
  int thread_id;
  CoroBase::DualEvent event;
};

template <class TaskSeq>
static inline void CollectDrainedDualEvents(
    std::vector<DrainedDualEventRecord>& drained, const TaskSeq& tasks,
    int thread_id) {
  for (size_t task_index = 0; task_index < tasks.size(); ++task_index) {
    const auto& task = tasks[task_index];
    if (!task->IsDual() || !task->HasDualEvents()) {
      continue;
    }

    auto events = task->DrainDualEvents();
    for (auto& e : events) {
      drained.push_back(
          DrainedDualEventRecord{e.seqno, task, thread_id, std::move(e)});
    }
  }
}

static inline void AppendCollectedDrainedDualEvents(
    std::vector<DualHistoryEvent>& seq,
    std::vector<DrainedDualEventRecord>& drained) {
  if (drained.empty()) {
    return;
  }

  std::sort(
      drained.begin(), drained.end(),
      [](const DrainedDualEventRecord& lhs, const DrainedDualEventRecord& rhs) {
        return lhs.seqno < rhs.seqno;
      });

  for (auto& rec : drained) {
    switch (rec.event.kind) {
      case CoroBase::DualEventKind::RequestResponse:
        seq.emplace_back(RequestResponse(rec.task, rec.thread_id));
        break;
      case CoroBase::DualEventKind::FollowUpInvoke:
        seq.emplace_back(FollowUpInvoke(rec.task, rec.thread_id));
        break;
      case CoroBase::DualEventKind::FollowUpResponse:
        seq.emplace_back(
            FollowUpResponse(rec.task, rec.event.result, rec.thread_id));
        break;
    }
  }
}

static inline void AppendDrainedDualEvents(std::vector<DualHistoryEvent>& seq,
                                           Strategy& strategy) {
  std::vector<DrainedDualEventRecord> drained;

  const auto& threads = strategy.GetTasks();
  for (size_t thread_id = 0; thread_id < threads.size(); ++thread_id) {
    CollectDrainedDualEvents(drained, threads[thread_id],
                             static_cast<int>(thread_id));
  }

  AppendCollectedDrainedDualEvents(seq, drained);
}

template <class Threads>
static inline void AppendDrainedDualEvents(std::vector<DualHistoryEvent>& seq,
                                           const Threads& threads) {
  std::vector<DrainedDualEventRecord> drained;

  for (size_t thread_id = 0; thread_id < threads.size(); ++thread_id) {
    CollectDrainedDualEvents(drained, threads[thread_id].tasks,
                             static_cast<int>(thread_id));
  }

  AppendCollectedDrainedDualEvents(seq, drained);
}


// StrategyScheduler generates different sequential histories (using Strategy)
// and then checks them with the ModelChecker
template <StrategyTaskVerifier Verifier>
struct StrategyScheduler : public SchedulerWithReplay {
  // max_switches represents the maximal count of switches. After this count
  // scheduler will end execution of the Run function
  StrategyScheduler(Strategy& sched_class, ModelChecker& checker,
                    std::vector<CustomRound> custom_rounds,
                    PrettyPrinter& pretty_printer, size_t max_tasks,
                    size_t max_rounds, bool minimize, size_t exploration_runs,
                    size_t minimization_runs,
                    DeadlockPolicy deadlock_policy = DeadlockPolicy::Fail)
      : strategy(sched_class),
        checker(checker),
        custom_rounds(std::move(custom_rounds)),
        pretty_printer(pretty_printer),
        max_tasks(max_tasks),
        max_rounds(max_rounds),
        should_minimize_history(minimize),
        exploration_runs(exploration_runs),
        minimization_runs(minimization_runs),
        deadlock_policy(deadlock_policy) {}

  // Run returns full unliniarizable history if such a history is found. Full
  // history is a history with all events, where each element in the vector is a
  // Resume operation on the corresponding task
  Scheduler::Result Run() override {
    for (size_t j = 0; j < custom_rounds.size() + max_rounds; ++j) {
      bool is_running_custom_scenarios = (j < custom_rounds.size());
      Result histories;

      if (is_running_custom_scenarios) {
        log() << "explore custom round: " << j << "\n";
        strategy.SetCustomRound(custom_rounds[j]);
        histories = ExploreRound(exploration_runs, true);
      } else {
        size_t i = j - custom_rounds.size();
        log() << "run round: " << i << "\n";
        debug(stderr, "run round: %zu\n", i);
        histories = RunRound();
      }

      if (HasTestFailure()) {
        return std::nullopt;
      }

      if (histories.has_value()) {
        auto& [full_history, sequential_history, reason] = histories.value();
        int threads_num = GetStrategyThreadsCount();

        if (should_minimize_history) {
          log() << "Full nonlinear scenario: \n";
          pretty_printer.PrettyPrint(sequential_history, threads_num, log());

          if (histories->reason == NonLinearizableHistory::Reason::DEADLOCK) {
            log() << "Skipping replay minimization for deadlock.\n";
          } else {
            log() << "Minimizing same interleaving...\n";
            Minimize(histories.value(), SameInterleavingMinimizor());
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(sequential_history, threads_num, log());

            log() << "Minimizing with rescheduling (exploration runs: "
                  << exploration_runs << ")...\n";
            Minimize(histories.value(),
                     StrategyExplorationMinimizor(exploration_runs));
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(sequential_history, threads_num, log());

            log() << "Minimizing with smart minimizor (exploration runs: "
                  << exploration_runs
                  << ", minimization runs: " << minimization_runs << ")...\n";
            Minimize(histories.value(),
                     SmartMinimizor(exploration_runs, minimization_runs,
                                    pretty_printer));
          }
        }

        return histories;
      }
      log() << "===============================================\n\n";
      log().flush();
      strategy.StartNextRound();
    }

    return std::nullopt;
  }

  size_t GetStrategyThreadsCount() const override {
    return strategy.GetThreadsCount();
  }

 protected:
  Result RunRound() override {
    SeqHistory sequential_history;
    FullHistoryWithThreads full_history;

    bool deadlock_detected{false};
    size_t started_tasks = 0;
    size_t finished_tasks = 0;

    strategy.SetAllowNewTasks(true);

    for (;;) {
      if (started_tasks >= max_tasks) {
        strategy.SetAllowNewTasks(false);

        auto [has_unfinished, has_runnable] =
            GetUnfinishedAndRunnable(strategy, /*allowed_ids*/ nullptr);

        if (!has_unfinished) {
          break;
        }
        if (!has_runnable) {
          deadlock_detected = true;
          break;
        }
      }

      auto t = strategy.Next();
      if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
        if (*f == DEADLOCK) {
          deadlock_detected = true;
          break;
        }
        if (*f == NEED_REPLAY) {
          log() << "restarted round\n";
          strategy.StartNextRound();
          sequential_history.clear();
          full_history.clear();
          finished_tasks = 0;
          continue;
        }
        if (*f == EXHAUSTED_INTERLEAVINGS) {
          return std::nullopt;
        }
      }
      auto [next_task, is_new, thread_id] = std::get<TaskWithMetaData>(t);
      next_task->clearWakeupCondition();

      // fill the sequential history
      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
        ++started_tasks;
      }

      next_task->Resume(thread_id);
      if (HasTestFailure()) {
        return std::nullopt;
      }
      strategy.UpdateSimulatorState(thread_id, sequential_history, full_history);
      if (next_task->IsReturned()) {
        ++finished_tasks;
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    pretty_printer.PrettyPrint(sequential_history, GetStrategyThreadsCount(),
                               log());

    if (deadlock_detected) {
      if (deadlock_policy != DeadlockPolicy::Fail) {
        if (!checker.Check(sequential_history)) {
          return NonLinearizableHistory(
              ConvFullHistWithThreadToFullHist(full_history), sequential_history,
              NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
        }

        if (deadlock_policy == DeadlockPolicy::Check) {
          return std::nullopt;
        }

        // TODO(bitree2004):: only for PCT, Random. TLA, RR
        const int k = static_cast<int>(exploration_runs);
        auto alt = ExploreRound(k, false);

        if (alt.has_value()) {
          return alt;
        }

        return std::nullopt;
      }
      if (!IsReportableDeadlockHistory(sequential_history)) {
        return std::nullopt;
      }
      return NonLinearizableHistory(
          ConvFullHistWithThreadToFullHist(full_history), sequential_history,
          NonLinearizableHistory::Reason::DEADLOCK);
    }

    if (!checker.Check(sequential_history)) {
      return NonLinearizableHistory(
          ConvFullHistWithThreadToFullHist(full_history), sequential_history,
          NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
    }

    return std::nullopt;
  }

  Result ExploreRound(int runs, bool log_each_interleaving) override {
    for (int i = 0; i < runs; ++i) {
      strategy.ResetCurrentRound();
      SeqHistory sequential_history;
      FullHistory full_history;

      bool deadlock_detected{false};

      std::unordered_set<int> started_ids;
      started_ids.reserve(strategy.GetTotalTasksCount());

      for (int tasks_to_run = strategy.GetValidTasksCount();
           tasks_to_run > 0;) {
        auto t = strategy.NextSchedule();
        if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
          if (*f == DEADLOCK) {
            deadlock_detected = true;
            break;
          }
        }

        auto [next_task, is_new, thread_id] = std::get<TaskWithMetaData>(t);;
        (void)is_new;

        const bool first_start_in_explore =
            !started_ids.contains(next_task->GetId());

        if (first_start_in_explore) {
          auto ctx = BuildReplayStartContext(strategy, started_ids);

          if (!strategy.VerifyTaskStart(next_task,
                                        thread_id, ctx)) {
            deadlock_detected = true;
            break;
          }

          strategy.OnVerifierTaskStart(next_task,
                                       static_cast<size_t>(thread_id));
          started_ids.insert(next_task->GetId());
          sequential_history.emplace_back(Invoke(next_task, thread_id));
        } else if (!next_task->IsReturned() &&
                   !strategy.VerifyExistingTask(
                       next_task, static_cast<size_t>(thread_id))) {
          deadlock_detected = true;
          break;
        }
        full_history.emplace_back(next_task);

        next_task->Resume(thread_id);
        if (HasTestFailure()) {
          return std::nullopt;
        }
        if (next_task->IsReturned()) {
          --tasks_to_run;
          strategy.OnVerifierTaskFinish(next_task, thread_id);

          auto result = next_task->GetRetVal();
          sequential_history.emplace_back(
              Response(next_task, result, thread_id));
        }
      }

      if (log_each_interleaving) {
        pretty_printer.PrettyPrint(sequential_history,
                                   GetStrategyThreadsCount(), log());
        log() << "\n";
      }

      if (deadlock_detected) {
        if (deadlock_policy != DeadlockPolicy::Fail) {
          if (checker.Check(sequential_history)) {
            continue;
          }
          return NonLinearizableHistory(
              full_history, sequential_history,
              NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
        }

        if (!IsReportableDeadlockHistory(sequential_history)) {
          continue;
        }

        return NonLinearizableHistory(full_history, sequential_history,
                                      NonLinearizableHistory::Reason::DEADLOCK);
      }

      if (!checker.Check(sequential_history)) {
        return NonLinearizableHistory(
            full_history, sequential_history,
            NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
      }
    }

    return std::nullopt;
  }

  Result ReplayRound(const std::vector<int>& tasks_ordering,
                     ReplayMode mode) override {
    strategy.ResetCurrentRound();

    std::unordered_set<int> in_ordering;
    in_ordering.reserve(tasks_ordering.size());
    for (int id : tasks_ordering) in_ordering.insert(id);

    FullHistory full_history;
    SeqHistory sequential_history;

    std::unordered_map<int, size_t> last_pos;
    last_pos.reserve(tasks_ordering.size());
    for (size_t i = 0; i < tasks_ordering.size(); ++i) {
      last_pos[tasks_ordering[i]] = i;
    }

    std::unordered_set<int> started_tasks;
    started_tasks.reserve(tasks_ordering.size());

    std::vector<bool> thread_cutoff(strategy.GetThreadsCount(), false);

    for (size_t step = 0; step < tasks_ordering.size(); ++step) {
      int next_task_id = tasks_ordering[step];

      auto task_info = strategy.GetTask(next_task_id);

      if (!task_info.has_value()) {
        std::cerr << "No task with id " << next_task_id << " exists in round\n";
        throw std::runtime_error("Invalid task id");
      }

      auto [next_task, thread_id] = task_info.value();

      if (thread_cutoff[thread_id]) {
        continue;
      }

      bool is_new = !started_tasks.contains(next_task_id);

      if (is_new) {
        auto ctx = BuildReplayStartContext(strategy, started_tasks);

        if (!strategy.VerifyTaskStart(next_task, static_cast<size_t>(thread_id),
                                      ctx)) {
          thread_cutoff[thread_id] = true;
          continue;
        }

        strategy.OnVerifierTaskStart(next_task, static_cast<size_t>(thread_id));
        started_tasks.insert(next_task_id);
      } else if (!next_task->IsReturned() &&
                 !strategy.VerifyExistingTask(next_task,
                                              static_cast<size_t>(thread_id))) {
        thread_cutoff[thread_id] = true;
        continue;
      }

      next_task->clearWakeupCondition();

      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      full_history.emplace_back(next_task);

      if (next_task->IsReturned()) continue;

      const bool is_last = (last_pos[next_task_id] == step);
      const bool forced_terminate =
          mode == ReplayMode::CompleteOnLast && is_last;
      if (forced_terminate) {
        next_task->Terminate();
      } else {
        next_task->Resume(thread_id);
        if (HasTestFailure()) {
          return std::nullopt;
        }
      }

      if (next_task->IsReturned()) {
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        if (!forced_terminate) {
          auto result = next_task->GetRetVal();
          sequential_history.emplace_back(
              Response(next_task, result, thread_id));
        }
      }
    }

    // Deadlock-friendly replay: detect "stuck" after ordering without forcing
    // completion.
    if (mode == ReplayMode::NoForceComplete) {
      auto [has_unfinished, has_runnable] =
          GetUnfinishedAndRunnable(strategy, &in_ordering);
      if (has_unfinished && !has_runnable) {
        if (!checker.Check(sequential_history)) {
          return NonLinearizableHistory(
              full_history, sequential_history,
              NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
        }
        if (deadlock_policy != DeadlockPolicy::Fail) {
          return std::nullopt;
        }
        if (!IsReportableDeadlockHistory(sequential_history)) {
          return std::nullopt;
        }
        return NonLinearizableHistory(full_history, sequential_history,
                                      NonLinearizableHistory::Reason::DEADLOCK);
      }
    }

    if (!checker.Check(sequential_history)) {
      return NonLinearizableHistory(
          full_history, sequential_history,
          NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
    }

    return std::nullopt;
  }

  Strategy& GetStrategy() const override { return strategy; }

  // Minimizes number of tasks in the nonlinearized history preserving threads
  // interleaving. Modifies argument `nonlinear_history`.
  void Minimize(NonLinearizableHistory& nonlinear_history,
                const RoundMinimizorT<HistoryEvent>& minimizor) override {
    minimizor.Minimize(*this, nonlinear_history);
  }

 private:
  Strategy& strategy;
  ModelChecker& checker;
  std::vector<CustomRound> custom_rounds;
  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  size_t max_rounds;
  bool should_minimize_history;
  size_t exploration_runs;
  size_t minimization_runs;
  DeadlockPolicy deadlock_policy;
};

// DualStrategyScheduler builds dual histories and supports replay/minimization.
template <StrategyTaskVerifier Verifier>
struct DualStrategyScheduler : public DualSchedulerWithReplay {
  DualStrategyScheduler(Strategy& sched_class,
                        DualLinearizabilityChecker& checker,
                        PrettyPrinter& pretty_printer, size_t max_tasks,
                        size_t max_rounds, bool minimize,
                        size_t exploration_runs, size_t minimization_runs,
                        DeadlockPolicy deadlock_policy = DeadlockPolicy::Fail)
      : strategy(sched_class),
        checker(checker),
        pretty_printer(pretty_printer),
        max_tasks(max_tasks),
        max_rounds(max_rounds),
        should_minimize_history(minimize),
        exploration_runs(exploration_runs),
        minimization_runs(minimization_runs),
        deadlock_policy(deadlock_policy) {}

  Result Run() override {
    for (size_t i = 0; i < max_rounds; ++i) {
      log() << "\nrun round: " << i << "\n\n";
      auto res = RunRound();
      if (res.has_value()) {
        int threads_num = GetStrategyThreadsCount();
        if (should_minimize_history) {
          log() << "Full nonlinear scenario (DUAL):\n";
          pretty_printer.PrettyPrint(res->seq, threads_num, log());

          if (res->reason == NonLinearizableHistory::
                                 Reason::DEADLOCK) {
            log() << "Skipping replay minimization for deadlock.\n";
          } else {
            log() << "Minimizing same interleaving...\n";
            Minimize(*res, DualSameInterleavingMinimizor{});
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(res->seq, threads_num, log());

            log() << "Minimizing with rescheduling (exploration runs: "
                  << exploration_runs << ")...\n";
            Minimize(*res, DualStrategyExplorationMinimizor(
                               static_cast<int>(exploration_runs)));
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(res->seq, threads_num, log());

            log() << "Minimizing with smart minimizor (exploration runs: "
                  << exploration_runs
                  << ", minimization runs: " << minimization_runs << ")...\n";
            Minimize(*res,
                     DualSmartMinimizor(static_cast<int>(exploration_runs),
                                        static_cast<int>(minimization_runs),
                                        pretty_printer));
          }
        }
        return res;
      }
      log() << "===============================================\n\n";
      log().flush();
      strategy.StartNextRound();
    }
    return std::nullopt;
  }

  size_t GetStrategyThreadsCount() const override {
    return strategy.GetThreadsCount();
  }

 protected:
  void EmitStartEvent(SeqHistory& seq,
                             Task& task, bool is_new, int thread_id) {
    AppendDualStartEvent(seq, task, is_new, thread_id);
  }

  void DrainDual(Task& task, int thread_id,
                        SeqHistory& seq) {
    (void)task;
    (void)thread_id;
    AppendDrainedDualEvents(seq, strategy);
  }

  struct RollbackCandidate {
    int task_id;
    size_t thread_id;
    std::string wait_method;
    std::vector<std::string> progress_methods;
  };

  std::optional<RollbackCandidate> FindRollbackCandidate(
      const FullHistory& full) {
    std::unordered_set<int> seen;

    for (auto it = full.rbegin(); it != full.rend(); ++it) {
      Task task = it->get();
      const int task_id = task->GetId();
      if (!seen.insert(task_id).second) {
        continue;
      }
      if (strategy.IsTaskRemoved(task_id)) {
        continue;
      }

      auto task_info = strategy.GetTask(task_id);
      if (!task_info.has_value()) {
        continue;
      }

      auto [stored_task, thread_id_i] = task_info.value();
      if (stored_task->IsReturned() || !stored_task->IsBlocked()) {
        continue;
      }

      auto progress_methods =
          strategy.GetDeadlockProgressMethods(stored_task->GetName());
      progress_methods.erase(
          std::remove_if(progress_methods.begin(), progress_methods.end(),
                         [&](const std::string& method) {
                           return !strategy.HasTaskBuilder(method);
                         }),
          progress_methods.end());

      if (progress_methods.empty()) {
        continue;
      }

      return RollbackCandidate{
          task_id,
          static_cast<size_t>(thread_id_i),
          std::string(stored_task->GetName()),
          std::move(progress_methods),
      };
    }

    return std::nullopt;
  }

  Result TryRollbackDeadlock(
      const FullHistory& full) {
    auto candidate = FindRollbackCandidate(full);
    if (!candidate.has_value()) {
      return std::nullopt;
    }

    const int attempts = std::max(1, static_cast<int>(exploration_runs));
    for (int attempt = 0; attempt < attempts; ++attempt) {
      for (const auto& progress_method : candidate->progress_methods) {
        strategy.SetTaskRemoved(candidate->task_id, true);
        auto appended_task_id =
            strategy.AppendTaskForReplay(progress_method, candidate->thread_id);
        if (!appended_task_id.has_value()) {
          strategy.SetTaskRemoved(candidate->task_id, false);
          continue;
        }

        log() << "Rollback deadlock attempt: remove " << candidate->wait_method
              << "#" << candidate->task_id << ", append " << progress_method
              << "#" << *appended_task_id << " on thread "
              << candidate->thread_id << "\n";

        auto alt = ExploreRound(1, false);
        if (alt.has_value()) {
          return alt;
        }

        strategy.ResetExplorationState();
        (void)strategy.RemoveLastTaskForReplay(candidate->thread_id,
                                               *appended_task_id);
        strategy.SetTaskRemoved(candidate->task_id, false);
      }
    }

    return std::nullopt;
  }

  Result HandleDeadlock(
      const FullHistory& full,
      const SeqHistory& seq) {
    if (deadlock_policy != DeadlockPolicy::Fail) {
      if (!checker.Check(seq)) {
        return NonLinearizableHistory{
            full, seq,
            NonLinearizableHistory::Reason::
                NON_LINEARIZABLE_HISTORY};
      }

      if (deadlock_policy == DeadlockPolicy::Check) {
        return std::nullopt;
      }

      if (deadlock_policy == DeadlockPolicy::Rollback) {
        auto rollback_alt = TryRollbackDeadlock(full);
        if (rollback_alt.has_value()) {
          return rollback_alt;
        }
      }

      const int k = static_cast<int>(exploration_runs);
      auto alt = ExploreRound(k, false);
      if (alt.has_value()) return alt;
      return std::nullopt;
    }

    if (!IsReportableDeadlockHistory(seq)) {
      return std::nullopt;
    }

    return NonLinearizableHistory{
        full, seq,
        NonLinearizableHistory::Reason::DEADLOCK};
  }

  // Runs a round with some interleaving while generating it (dual mode)
  Result RunRound() override {
    SeqHistory seq;
    FullHistory full;

    bool deadlock_detected{false};

    size_t started_tasks = 0;

    strategy.SetAllowNewTasks(true);

    for (;;) {
      if (started_tasks >= max_tasks) {
        strategy.SetAllowNewTasks(false);

        auto [has_unfinished, has_runnable] =
            GetUnfinishedAndRunnable(strategy, /*allowed_ids*/ nullptr);

        if (!has_unfinished) {
          break;
        }
        if (!has_runnable) {
          deadlock_detected = true;
          break;
        }
      }

      auto t = strategy.Next();
      if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
        if (*f == DEADLOCK) {
          deadlock_detected = true;
          break;
        }
        if (*f == NEED_REPLAY) {
          log() << "restarted round\n";
          strategy.StartNextRound();
          seq.clear();
          full.clear();
          continue;
        }
      }
      auto [task, is_new, thread_id_sz] = std::get<TaskWithMetaData>(t);
      int thread_id = static_cast<int>(thread_id_sz);

      EmitStartEvent(seq, task, is_new, thread_id);
      if (is_new) {
        ++started_tasks;
      }

      full.emplace_back(task);

      task->Resume(thread_id);
      DrainDual(task, thread_id, seq);

      if (task->IsReturned()) {
        strategy.OnVerifierTaskFinish(task, thread_id_sz);

        if (!task->IsDual()) {
          auto result = task->GetRetVal();
          seq.emplace_back(Response(task, result, thread_id));
        }
      }
    }

    pretty_printer.PrettyPrint(seq, GetStrategyThreadsCount(), log());

    if (deadlock_detected) {
      return HandleDeadlock(full, seq);
    }

    if (!checker.Check(seq)) {
      return NonLinearizableHistory{
          full, seq,
          NonLinearizableHistory::Reason::
              NON_LINEARIZABLE_HISTORY};
    }

    return std::nullopt;
  }

  Result ExploreRound(
      int runs, bool log_each_interleaving = false) override {
    for (int i = 0; i < runs; ++i) {
      strategy.ResetExplorationState();

      SeqHistory seq;
      FullHistory full;

      bool deadlock_detected{false};

      std::unordered_set<int> started_ids;
      started_ids.reserve(strategy.GetTotalTasksCount());

      for (int tasks_to_run = strategy.GetValidTasksCount();
           tasks_to_run > 0;) {
        auto t = strategy.NextSchedule();
        if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
          if (*f == DEADLOCK) {
            deadlock_detected = true;
            break;
          }
        }

        auto [task, is_new, thread_id_sz] = std::get<TaskWithMetaData>(t);;
        int thread_id = static_cast<int>(thread_id_sz);

        const bool first_start_in_explore =
            !started_ids.contains(task->GetId());

        if (first_start_in_explore) {
          auto ctx = BuildReplayStartContext(strategy, started_ids);

          if (!strategy.VerifyTaskStart(task, thread_id_sz, ctx)) {
            deadlock_detected = true;
            break;
          }

          strategy.OnVerifierTaskStart(task, thread_id_sz);
          started_ids.insert(task->GetId());
        } else {
          // Existing task may no longer be legal in the current
          // replay/exploration state (for example, old unlock after a lock did
          // not complete normally).
          if (!task->IsReturned() &&
              !strategy.VerifyExistingTask(task, thread_id_sz)) {
            deadlock_detected = true;
            break;
          }
        }

        EmitStartEvent(seq, task, first_start_in_explore, thread_id);
        full.emplace_back(task);

        task->Resume(thread_id);

        DrainDual(task, thread_id, seq);

        if (task->IsReturned()) {
          tasks_to_run--;
          strategy.OnVerifierTaskFinish(task, thread_id_sz);

          if (!task->IsDual()) {
            auto result = task->GetRetVal();
            seq.emplace_back(Response(task, result, thread_id));
          }
        }
      }

      if (deadlock_detected) {
        if (deadlock_policy != DeadlockPolicy::Fail) {
          if (checker.Check(seq)) {
            continue;
          }
          return NonLinearizableHistory{
              full, seq,
              NonLinearizableHistory::Reason::
                  NON_LINEARIZABLE_HISTORY};
        }

        if (!IsReportableDeadlockHistory(seq)) {
          continue;
        }

        return NonLinearizableHistory{
            full, seq,
            NonLinearizableHistory::Reason::DEADLOCK};
      }

      if (log_each_interleaving) {
        pretty_printer.PrettyPrint(seq, GetStrategyThreadsCount(), log());
        log() << "\n";
      }

      if (!checker.Check(seq)) {
        return NonLinearizableHistory{
            full, seq,
            NonLinearizableHistory::Reason::
                NON_LINEARIZABLE_HISTORY};
      }
    }

    return std::nullopt;
  }

  Result ReplayRound(
      const std::vector<int>& tasks_ordering, ReplayMode mode) override {
    strategy.ResetCurrentRound();

    std::unordered_set<int> in_ordering;
    in_ordering.reserve(tasks_ordering.size());
    for (int id : tasks_ordering) {
      in_ordering.insert(id);
    }

    FullHistory full;
    SeqHistory seq;

    std::unordered_map<int, size_t> last_pos;
    last_pos.reserve(tasks_ordering.size());
    for (size_t i = 0; i < tasks_ordering.size(); ++i) {
      last_pos[tasks_ordering[i]] = i;
    }

    std::unordered_set<int> started;
    started.reserve(tasks_ordering.size());

    std::vector<bool> thread_cutoff(strategy.GetThreadsCount(), false);

    for (size_t step = 0; step < tasks_ordering.size(); ++step) {
      int task_id = tasks_ordering[step];

      auto task_info = strategy.GetTask(task_id);
      if (!task_info.has_value()) {
        std::cerr << "No task with id " << task_id << " exists in round\n";
        throw std::runtime_error("Invalid task id");
      }

      auto [task, thread_id_i] = task_info.value();
      int thread_id = thread_id_i;

      if (thread_cutoff[thread_id]) {
        continue;
      }

      bool is_new = !started.contains(task_id);

      if (is_new) {
        auto ctx = BuildReplayStartContext(strategy, started);

        if (!strategy.VerifyTaskStart(task, static_cast<size_t>(thread_id_i),
                                      ctx)) {
          thread_cutoff[thread_id] = true;
          continue;
        }

        strategy.OnVerifierTaskStart(task, static_cast<size_t>(thread_id_i));
        started.insert(task_id);
      } else {
        // Existing task may no longer be legal in this replay state.
        if (!task->IsReturned() &&
            !strategy.VerifyExistingTask(task,
                                         static_cast<size_t>(thread_id_i))) {
          thread_cutoff[thread_id] = true;
          continue;
        }
      }

      task->clearWakeupCondition();

      EmitStartEvent(seq, task, is_new, thread_id);
      full.emplace_back(task);

      if (task->IsReturned()) {
        continue;
      }

      const bool is_last = (last_pos[task_id] == step);
      const bool forced_terminate =
          mode == ReplayMode::CompleteOnLast && is_last;
      if (forced_terminate) {
        task->Terminate();
      } else {
        task->Resume(thread_id);
      }

      DrainDual(task, thread_id, seq);

      if (task->IsReturned()) {
        strategy.OnVerifierTaskFinish(task, static_cast<size_t>(thread_id_i));

        if (!forced_terminate && !task->IsDual()) {
          auto result = task->GetRetVal();
          seq.emplace_back(Response(task, result, thread_id));
        }
      }
    }

    if (mode == ReplayMode::NoForceComplete) {
      auto [has_unfinished, has_runnable] =
          GetUnfinishedAndRunnable(strategy, &in_ordering);
      if (has_unfinished && !has_runnable) {
        if (!checker.Check(seq)) {
          return NonLinearizableHistory{
              full, seq,
              NonLinearizableHistory::Reason::
                  NON_LINEARIZABLE_HISTORY};
        }
        if (deadlock_policy != DeadlockPolicy::Fail) {
          return std::nullopt;
        }
        if (!IsReportableDeadlockHistory(seq)) {
          return std::nullopt;
        }
        return NonLinearizableHistory{
            full, seq,
            NonLinearizableHistory::Reason::DEADLOCK};
      }
    }

    if (!checker.Check(seq)) {
      return NonLinearizableHistory{
          full, seq,
          NonLinearizableHistory::Reason::
              NON_LINEARIZABLE_HISTORY};
    }

    return std::nullopt;
  }

  Strategy& GetStrategy() const override { return strategy; }

  void Minimize(NonLinearizableHistory& nonlinear_history,
                const RoundMinimizorT<DualHistoryEvent>& minimizor) override {
    minimizor.Minimize(*this, nonlinear_history);
  }

 private:
  Strategy& strategy;
  DualLinearizabilityChecker& checker;
  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  size_t max_rounds;
  bool should_minimize_history;
  size_t exploration_runs;
  size_t minimization_runs;
  DeadlockPolicy deadlock_policy;
};

}  // namespace ltest
