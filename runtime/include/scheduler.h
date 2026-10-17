#pragma once
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <functional>
#include <iterator>
#include <limits>
#include <numeric>
#include <optional>
#include <random>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "attempt.h"
#include "coro_ctx_guard.h"
#include "custom_round.h"
#include "lib.h"
#include "lincheck.h"
#include "lincheck_dual.h"
#include "logger.h"
#include "os_simulator.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"
#include "stable_vector.h"
#include "strategy_shared_state.h"
#include "wmm/wmm.h"
#include "workload_policy.h"

namespace ltest {

struct TaskWithMetaData {
  Task& task;
  size_t thread_id;
};

enum StrategyNextScheduleFailure { DEADLOCK, NEED_REPLAY };

using StrategyNextResult =
    std::variant<StrategyNextScheduleFailure, TaskWithMetaData>;

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

  void OnFinished(Task&, size_t) {
    // intentionally do nothing
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
  virtual BaseStrategySharedState& GetSharedState() = 0;
  // Called once in each fork child.
  virtual void StartNextRound(const CustomRound* custom_round = nullptr) = 0;
  virtual bool IsExhausted() { return false; }
  virtual std::optional<size_t> NextThreadId() = 0;

  virtual StrategyNextResult Next() = 0;

  // Returns the same data as `Next` method. However, it does not generate the
  // round by inserting new tasks in it, but schedules the threads according to
  // the strategy policy with previously generated and saved round (used for
  // round replaying functionality)
  virtual StrategyNextResult NextSchedule() = 0;

  // Returns { task, its thread id } by task id. (TODO: make it `const` method)
  virtual std::optional<std::tuple<Task&, int>> GetTask(int task_id) = 0;

  // TODO: abstract this method more (returning `vector<StableVector<...>>` is
  // not good)
  virtual const std::vector<StableVector<Task>>& GetTasks() const = 0;

  void SetAllowNewTasks(bool v) { allow_new_tasks_ = v; }
  bool AllowNewTasks() const { return allow_new_tasks_; }

  virtual ~Strategy() = default;

  // Sets the custom round during StartNextRound().
  virtual void SetCustomRound(const CustomRound& custom_round) = 0;

  // Returns the number of threads
  virtual int GetThreadsCount() const = 0;

  virtual void UpdateSimulatorState(size_t thread_id) = 0;
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

  virtual std::optional<std::uint64_t> GetBuilderId(
      std::string_view method_name) const = 0;

  virtual std::vector<std::string> GetDeadlockProgressMethods(
      std::string_view wait_method) const = 0;

 protected:
  // For current round returns first task index in thread which is greater
  // than `round_schedule[thread]` or the same index if the task is not finished
  virtual int GetNextTaskInThread(int thread_index) const = 0;

  void ResetWmmGraph(int threads_count) {
    if (ltest::wmm::wmm_enabled) {
      wmm_graph.Reset(threads_count);
    }
  }

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
                          TargetFactory target_factory)
      : threads_count(threads_count),
        constructors(std::move(constructors)),
        constructor_order(this->constructors.size()),
        target_factory(std::move(target_factory)) {
    std::iota(constructor_order.begin(), constructor_order.end(), 0);
  }

  void StartNextRound(const CustomRound* custom_round = nullptr) override {
    sched_checker = Verifier{};
    if (custom_round) {
      for (const auto& thread : custom_round->threads)
        for (const auto& builder : thread) replay_builders.push_back(&builder);
      if (attempt_session->request.kind == AttemptKind::Generate) {
        SetCustomRound(*custom_round);
        return;
      }
      threads_count = custom_round->threads.size();
    }
    // must be called before instantiating `TargetObj`
    ResetWmmGraph(this->threads_count);
    round_schedule.resize(threads_count, -1);
    ConstructTarget();

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      threads.emplace_back();
    }

    current_max_thread_id = threads_count ? threads_count - 1 : 0;
    verifier_hooks::OnRoundStart(sched_checker, threads_count);
    SetAllowNewTasks(attempt_session->request.kind == AttemptKind::Generate);
  }

 protected:
  std::optional<StrategyNextResult> GetReplayTask() {
    const auto& request = attempt_session->request;
    if (request.kind == AttemptKind::Generate) return std::nullopt;
    if (!RefreshReplayTasks()) return NEED_REPLAY;
    if (request.kind != AttemptKind::FixedReplay) return std::nullopt;
    const int id = request.ordering[attempt_session->report.ordering.size()];
    Task* task = BuildReplayOperation(id);
    if (!task) return NEED_REPLAY;
    auto info = GetTask(id);
    return TaskWithMetaData{*task, static_cast<size_t>(std::get<1>(*info))};
  }

  Task* BuildReplayOperation(int id) {
    if (auto existing = GetTask(id)) return &std::get<0>(*existing);
    const auto* op = attempt_session->Source(id);
    if (!op || op->id < 0 || op->thread >= threads.size()) return nullptr;
    auto& thread = threads[op->thread];
    if (op->builder >= constructors.size() + replay_builders.size())
      return nullptr;
    thread.emplace_back(BuildTask(op->builder, op->thread, op->id));
    return &thread.back();
  }

  bool RefreshReplayTasks() {
    if (attempt_session->request.kind == AttemptKind::FixedReplay) {
      const size_t step = attempt_session->report.ordering.size();
      for (const auto& op : attempt_session->request.operations) {
        if (op.creation_step > step) break;
        if (!GetTask(op.id) && op.id >= 0 && !BuildReplayOperation(op.id))
          return false;
      }
      return true;
    }
    for (const auto& op : attempt_session->request.operations) {
      if (GetTask(op.id) || op.id < 0 || op.thread >= threads.size()) continue;
      auto& thread = threads[op.thread];
      if (!thread.empty() && !thread.back()->IsReturned()) continue;
      bool earlier_pending = false;
      for (const auto& prev : attempt_session->request.operations) {
        if (prev.id == op.id) break;
        if (prev.thread == op.thread && !GetTask(prev.id))
          earlier_pending = true;
      }
      if (earlier_pending) continue;
      if (!BuildReplayOperation(op.id)) return false;
    }
    return true;
  }

 public:
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

  std::optional<std::uint64_t> GetBuilderId(
      std::string_view method_name) const override {
    for (size_t i = 0; i < constructors.size(); ++i)
      if (constructors[i].GetName() == method_name) return i;
    return std::nullopt;
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

  void SetCustomRound(const CustomRound& custom_round) override {
    size_t custom_threads_count = custom_round.threads.size();
    current_max_thread_id = custom_threads_count ? custom_threads_count - 1 : 0;

    // custom round threads count might differ from the generated rounds
    this->threads.resize(custom_threads_count);
    this->round_schedule.resize(custom_threads_count, -1);
    ResetWmmGraph(custom_threads_count);
    this->ConstructTarget();

    size_t builder_index = constructors.size();
    for (size_t current_thread = 0; current_thread < custom_threads_count;
         ++current_thread) {
      auto& builders = custom_round.threads[current_thread];
      StableVector<Task> thread_tasks;
      for (size_t i = 0; i < builders.size(); ++i) {
        auto task = BuildTask(builder_index++, current_thread, new_task_id++);
        thread_tasks.emplace_back(task);
      }
      this->threads[current_thread] = std::move(thread_tasks);
    }
    ltest::verifier_hooks::OnRoundStart(this->sched_checker,
                                        custom_threads_count);
    SetAllowNewTasks(false);
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

  StrategyNextResult Next() override {
    auto opt_thread_index = NextThreadId();
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
      std::shuffle(constructor_order.begin(), constructor_order.end(),
                   GetSharedState().rng);
      size_t verified_constructor = static_cast<size_t>(-1);

      for (size_t i : constructor_order) {
        const TaskBuilder& constructor = this->constructors.at(i);

        if (verifier_hooks::VerifyStart(sched_checker, constructor.GetName(),
                                        thread_index, ctx) &&
            this->sched_checker.Verify(constructor.GetName(), thread_index,
                                       is_new)) {
          verified_constructor = i;
          break;
        }
      }

      if (verified_constructor == static_cast<size_t>(-1)) {
        return DEADLOCK;
      }

      const TaskBuilder& chosen = this->constructors[verified_constructor];
      const std::string& method_name = chosen.GetName();

      Task task = BuildTask(verified_constructor, thread_index, new_task_id++);
      verifier_hooks::OnTaskStarted(sched_checker, method_name, thread_index,
                                    task->GetId());

      threads[thread_index].emplace_back(std::move(task));
    }

    return TaskWithMetaData{threads[thread_index].back(), thread_index};
  }

 protected:
  Task BuildTask(size_t builder, size_t thread_id, int task_id) {
    auto& report = attempt_session->report;
    OperationDescription op(task_id, builder, thread_id,
                            report.ordering.size());
    const auto& constructor =
        builder < constructors.size()
            ? constructors[builder]
            : *replay_builders[builder - constructors.size()];
    auto task = constructor.Build(state.get(), thread_id, task_id);
    report.operations.push_back(std::move(op));
    return task;
  }

  /// Returns the first unfinished task in threads[thread_index].
  int GetNextTaskInThread(int thread_index) const override {
    auto& thread = threads[thread_index];
    int task_index = round_schedule[thread_index];

    while (task_index < static_cast<int>(thread.size()) &&
           (task_index == -1 || thread[task_index]->IsReturned())) {
      task_index++;
    }

    return task_index;
  }

  void UpdateSimulatorState(size_t thread_id) override {
    this->UpdateOSState(thread_id);
    round_schedule.resize(threads.size(), -1);
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

  int new_task_id = 0;
  std::vector<const TaskBuilder*> replay_builders;
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
  std::vector<size_t> constructor_order;
  TargetFactory target_factory;

 private:
  void ConstructTarget() {
    CoroCtxGuard guard;
    TargetConstructionGuard target_construction_guard;
    state = target_factory();
  }
};

static bool HasPendingReplayTasks(Strategy& strategy,
                                  const AttemptRequest& request) {
  for (const auto& op : request.operations)
    if (!strategy.GetTask(op.id)) return true;
  return false;
}

static inline std::pair<bool, bool> GetUnfinishedAndRunnable(
    const Strategy& strategy) {
  bool has_unfinished = false;
  bool has_runnable = false;

  const auto& threads = strategy.GetTasks();
  for (int tid = 0; tid < static_cast<int>(threads.size()); ++tid) {
    const auto& thr = threads[tid];
    for (int i = 0; i < static_cast<int>(thr.size()); ++i) {
      const auto& t = thr[i];
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

// Checker vectors are temporary views of the report's child-local history.
template <class Event>
static std::vector<Event> BuildSequentialHistory(const AttemptReport& report,
                                                 Strategy& strategy) {
  std::vector<Event> seq;
  seq.reserve(report.events.size());
  for (const auto& event : report.events) {
    auto info = strategy.GetTask(event.operation);
    const auto& task = std::get<0>(*info);
    const int thread = std::get<1>(*info);
    switch (event.kind) {
      case DualHistoryEventKind::OrdinaryInvoke:
        seq.emplace_back(Invoke(task, thread));
        break;
      case DualHistoryEventKind::OrdinaryResponse:
        seq.emplace_back(Response(task, event.result, thread));
        break;
      case DualHistoryEventKind::RequestInvoke:
        if constexpr (std::is_same_v<Event, DualHistoryEvent>)
          seq.emplace_back(RequestInvoke(task, thread));
        break;
      case DualHistoryEventKind::RequestResponse:
        if constexpr (std::is_same_v<Event, DualHistoryEvent>)
          seq.emplace_back(RequestResponse(task, thread));
        break;
      case DualHistoryEventKind::FollowUpInvoke:
        if constexpr (std::is_same_v<Event, DualHistoryEvent>)
          seq.emplace_back(FollowUpInvoke(task, thread));
        break;
      case DualHistoryEventKind::FollowUpResponse:
        if constexpr (std::is_same_v<Event, DualHistoryEvent>)
          seq.emplace_back(FollowUpResponse(task, event.result, thread));
        break;
    }
  }
  return seq;
}

static inline void AppendDualStartEvent(AttemptReport& report, Task& task) {
  if (task->IsDual()) {
    report.events.emplace_back(task->GetId(),
                               DualHistoryEventKind::RequestInvoke);
  } else {
    report.events.emplace_back(task->GetId(),
                               DualHistoryEventKind::OrdinaryInvoke);
  }
}

struct DrainedDualEventRecord {
  std::uint64_t seqno;
  Task task;
  CoroBase::DualEvent event;
};

template <class TaskSeq>
static inline void CollectDrainedDualEvents(
    std::vector<DrainedDualEventRecord>& drained, const TaskSeq& tasks) {
  for (size_t task_index = 0; task_index < tasks.size(); ++task_index) {
    const auto& task = tasks[task_index];
    if (!task->IsDual() || !task->HasDualEvents()) {
      continue;
    }

    auto events = task->DrainDualEvents();
    for (auto& e : events) {
      drained.push_back(DrainedDualEventRecord{e.seqno, task, std::move(e)});
    }
  }
}

static inline void AppendCollectedDrainedDualEvents(
    AttemptReport& report, std::vector<DrainedDualEventRecord>& drained) {
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
        report.events.emplace_back(rec.task->GetId(),
                                   DualHistoryEventKind::RequestResponse);
        break;
      case CoroBase::DualEventKind::FollowUpInvoke:
        report.events.emplace_back(rec.task->GetId(),
                                   DualHistoryEventKind::FollowUpInvoke);
        break;
      case CoroBase::DualEventKind::FollowUpResponse:
        report.events.emplace_back(rec.task->GetId(),
                                   DualHistoryEventKind::FollowUpResponse,
                                   rec.event.result);
        break;
    }
  }
}

static inline void AppendDrainedDualEvents(AttemptReport& report,
                                           Strategy& strategy) {
  std::vector<DrainedDualEventRecord> drained;

  const auto& threads = strategy.GetTasks();
  for (size_t thread_id = 0; thread_id < threads.size(); ++thread_id) {
    CollectDrainedDualEvents(drained, threads[thread_id]);
  }

  AppendCollectedDrainedDualEvents(report, drained);
}

// StrategyScheduler generates ordinary histories and checks them with the
// ModelChecker. Each Run executes one attempt in a fresh worker.
struct StrategyScheduler : public Scheduler {
  StrategyScheduler(Strategy& sched_class, ModelChecker& checker,
                    PrettyPrinter& pretty_printer, size_t max_tasks,
                    DeadlockPolicy deadlock_policy = DeadlockPolicy::Fail)
      : strategy(sched_class),
        checker(checker),
        pretty_printer(pretty_printer),
        max_tasks(max_tasks),
        deadlock_policy(deadlock_policy) {}

  size_t GetStrategyThreadsCount() const override {
    return strategy.GetThreadsCount();
  }
  DeadlockPolicy GetDeadlockPolicy() const override { return deadlock_policy; }

  void Run() override {
    auto& session = *attempt_session;
    auto& report = session.report;
    const auto& request = session.request;
    const bool replay = request.kind != AttemptKind::Generate;
    const bool fixed = request.kind == AttemptKind::FixedReplay;
    const bool generate = !replay && strategy.AllowNewTasks();
    std::unordered_set<int> started;
    bool deadlock_detected{false};
    for (;;) {
      if (fixed && report.ordering.size() >= request.ordering.size()) break;
      if (generate && started.size() >= max_tasks)
        strategy.SetAllowNewTasks(false);
      if (!fixed) {
        const bool unfinished = GetUnfinishedAndRunnable(strategy).first;
        if (!strategy.AllowNewTasks() && !unfinished &&
            (!replay || !HasPendingReplayTasks(strategy, request)))
          break;
      }
      auto t = generate ? strategy.Next() : strategy.NextSchedule();
      if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
        if (*f == NEED_REPLAY) {
          if (replay) {
            report.result = Success;
            return;
          }
          deadlock_detected = GetUnfinishedAndRunnable(strategy).first;
        } else if (*f == DEADLOCK)
          deadlock_detected = true;
        break;
      }
      auto [next_task, thread_id] = std::get<TaskWithMetaData>(t);
      const int id = next_task->GetId();
      const bool is_new = !started.contains(id);
      if (fixed && next_task->IsReturned()) {
        report.ordering.push_back(id);
        continue;
      }
      if (is_new) {
        if (fixed) {
          const auto& tasks = strategy.GetTasks()[thread_id];
          for (size_t i = 0; i < tasks.size(); ++i) {
            const auto& active = tasks[i];
            if (started.contains(active->GetId()) && !active->IsReturned()) {
              report.result = Success;
              return;
            }
          }
        }
        // Generation already checked/notified the verifier in Next().
        if (!generate) {
          auto ctx = BuildReplayStartContext(strategy, started);
          if (!strategy.VerifyTaskStart(next_task, thread_id, ctx)) {
            report.result = Success;
            return;
          }
          strategy.OnVerifierTaskStart(next_task, thread_id);
        }
        started.insert(id);
        report.events.emplace_back(id, DualHistoryEventKind::OrdinaryInvoke);
      }
      next_task->clearWakeupCondition();
      report.ordering.push_back(id);
      next_task->Resume(thread_id);
      {
        const size_t previous_events = report.events.size();
        strategy.UpdateSimulatorState(thread_id);
        // pthread invocation is observed at creation, before its first resume.
        for (size_t i = previous_events; i < report.events.size(); ++i)
          if (report.events[i].IsStart())
            started.insert(report.events[i].operation);
      }
      if (next_task->IsReturned()) {
        strategy.OnVerifierTaskFinish(next_task, thread_id);
        auto result = next_task->GetRetVal();
        report.events.emplace_back(id, DualHistoryEventKind::OrdinaryResponse,
                                   result);
      }
      if (HasTestFailure()) break;
    }
    if (fixed) {
      auto [unfinished, runnable] = GetUnfinishedAndRunnable(strategy);
      deadlock_detected = unfinished && !runnable;
    }
    for (auto& op : report.operations) {
      auto task = strategy.GetTask(op.id);
      if (!task) continue;
      const auto& coro = std::get<0>(*task);
      if (!coro->IsReturned() && coro->IsBlocked())
        op.progress_methods =
            strategy.GetDeadlockProgressMethods(coro->GetName());
    }
    auto seq = BuildSequentialHistory<event_t>(report, strategy);
    for (size_t i = 0; i < seq.size(); ++i)
      report.events[i].text = pretty_printer.FormatEvent(seq[i]);
    if (HasTestFailure()) {
      report.result = AssertionFailure;
      return;
    }
    if (!checker.Check(seq)) {
      report.result = NonLinearizable;
      return;
    }
    if (deadlock_detected) {
      report.result = Deadlock;
      if (deadlock_policy == DeadlockPolicy::Fail && !MeaningfulFailure(report))
        report.result = Success;
    }
  }

 private:
  Strategy& strategy;
  ModelChecker& checker;
  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  DeadlockPolicy deadlock_policy;
};

// DualStrategyScheduler builds dual histories and checks their event ordering.
struct DualStrategyScheduler : public DualScheduler {
  DualStrategyScheduler(Strategy& sched_class,
                        DualLinearizabilityChecker& checker,
                        PrettyPrinter& pretty_printer, size_t max_tasks,
                        DeadlockPolicy deadlock_policy = DeadlockPolicy::Fail)
      : strategy(sched_class),
        checker(checker),
        pretty_printer(pretty_printer),
        max_tasks(max_tasks),
        deadlock_policy(deadlock_policy) {}

  size_t GetStrategyThreadsCount() const override {
    return strategy.GetThreadsCount();
  }
  DeadlockPolicy GetDeadlockPolicy() const override { return deadlock_policy; }

  void Run() override {
    auto& session = *attempt_session;
    auto& report = session.report;
    const auto& request = session.request;
    const bool replay = request.kind != AttemptKind::Generate;
    const bool fixed = request.kind == AttemptKind::FixedReplay;
    std::unordered_set<int> started;
    bool deadlock_detected{false};
    for (;;) {
      if (fixed && report.ordering.size() >= request.ordering.size()) break;
      if (!replay && started.size() >= max_tasks)
        strategy.SetAllowNewTasks(false);
      if (!fixed) {
        const bool unfinished = GetUnfinishedAndRunnable(strategy).first;
        if ((replay || !strategy.AllowNewTasks()) && !unfinished &&
            (!replay || !HasPendingReplayTasks(strategy, request)))
          break;
      }
      auto t = replay ? strategy.NextSchedule() : strategy.Next();
      if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
        if (*f == NEED_REPLAY) {
          if (replay) {
            report.result = Success;
            return;
          }
          deadlock_detected = GetUnfinishedAndRunnable(strategy).first;
        } else if (*f == DEADLOCK)
          deadlock_detected = true;
        break;
      }
      auto [task, thread_id] = std::get<TaskWithMetaData>(t);
      const int id = task->GetId();
      const bool is_new = !started.contains(id);
      if (fixed && task->IsReturned()) {
        report.ordering.push_back(id);
        continue;
      }
      if (is_new) {
        if (fixed) {
          const auto& tasks = strategy.GetTasks()[thread_id];
          for (size_t i = 0; i < tasks.size(); ++i) {
            const auto& active = tasks[i];
            if (started.contains(active->GetId()) && !active->IsReturned()) {
              report.result = Success;
              return;
            }
          }
        }
        // Generation already checked/notified the verifier in Next().
        if (replay) {
          auto ctx = BuildReplayStartContext(strategy, started);
          if (!strategy.VerifyTaskStart(task, thread_id, ctx)) {
            report.result = Success;
            return;
          }
          strategy.OnVerifierTaskStart(task, thread_id);
        }
        started.insert(id);
        AppendDualStartEvent(report, task);
      }
      task->clearWakeupCondition();
      report.ordering.push_back(id);
      task->Resume(thread_id);
      AppendDrainedDualEvents(report, strategy);
      if (task->IsReturned()) {
        strategy.OnVerifierTaskFinish(task, thread_id);
        auto result = task->GetRetVal();
        if (!task->IsDual())
          report.events.emplace_back(id, DualHistoryEventKind::OrdinaryResponse,
                                     result);
      }
      if (HasTestFailure()) break;
    }
    if (fixed) {
      auto [unfinished, runnable] = GetUnfinishedAndRunnable(strategy);
      deadlock_detected = unfinished && !runnable;
    }
    for (auto& op : report.operations) {
      auto task = strategy.GetTask(op.id);
      if (!task) continue;
      const auto& coro = std::get<0>(*task);
      if (!coro->IsReturned() && coro->IsBlocked())
        op.progress_methods =
            strategy.GetDeadlockProgressMethods(coro->GetName());
    }
    auto seq = BuildSequentialHistory<event_t>(report, strategy);
    for (size_t i = 0; i < seq.size(); ++i)
      report.events[i].text = pretty_printer.FormatEvent(seq[i]);
    if (HasTestFailure()) {
      report.result = AssertionFailure;
      return;
    }
    if (!checker.Check(seq)) {
      report.result = NonLinearizable;
      return;
    }
    if (deadlock_detected) {
      report.result = Deadlock;
      if (deadlock_policy == DeadlockPolicy::Fail && !MeaningfulFailure(report))
        report.result = Success;
    }
  }

 private:
  Strategy& strategy;
  DualLinearizabilityChecker& checker;
  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  DeadlockPolicy deadlock_policy;
};

}  // namespace ltest
