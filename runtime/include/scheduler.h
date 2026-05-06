#pragma once
#include <algorithm>
#include <cassert>
#include <functional>
#include <iterator>
#include <limits>
#include <optional>
#include <random>
#include <string>
#include <string_view>
#include <utility>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>
#include <cstdio>

#include "custom_round.h"
#include "lib.h"
#include "lincheck.h"
#include "lincheck_dual.h"
#include "logger.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"
#include "stable_vector.h"
#include "workload_policy.h"

struct TaskWithMetaData {
  Task& task;
  bool is_new;
  size_t thread_id;
};

/// StrategyTaskVerifier is required for scheduling only allowed tasks
/// Some data structures doesn't allow us to schedule one tasks before another
/// e.g. Mutex -- we are not allowed to schedule unlock before lock call, it is
/// UB.
template <typename T>
concept StrategyTaskVerifier = requires(T a) {
  { a.Verify(std::declval<const std::string&>(), size_t()) } -> std::same_as<bool>;
  { a.OnFinished(std::declval<Task&>(), size_t()) } -> std::same_as<void>;
  { a.ReleaseTask(size_t()) } -> std::same_as<std::optional<std::string>>;
};

namespace ltest::verifier_hooks {

template <class V>
void OnRoundStart(V& v, std::size_t threads) {
  if constexpr (requires { v.OnRoundStart(threads); }) {
    v.OnRoundStart(threads);
  }
}

template <class V>
void OnTaskStarted(V& v, const std::string& method, std::size_t thread_id, int task_id) {
  if constexpr (requires { v.OnTaskStarted(method, thread_id, task_id); }) {
    v.OnTaskStarted(method, thread_id, task_id);
  }
}

template <class V>
bool VerifyStart(V& v, const std::string& method, std::size_t thread_id,
                 const ltest::StartContext& ctx) {
  if constexpr (requires { v.VerifyStart(method, thread_id, ctx); }) {
    return v.VerifyStart(method, thread_id, ctx);
  } else {
    return true;
  }
}

} // namespace ltest::verifier_hooks

// Strategy is the general strategy interface which decides which task
// will be the next one it can be implemented by different strategies, such as:
// randomized/tla/fair
struct Strategy {
  virtual std::optional<size_t> NextThreadId() = 0;

  virtual std::optional<TaskWithMetaData> Next() = 0;

  virtual void TerminateTasks() = 0;

  virtual void AbortTasksForFailure() = 0;

  virtual void ResetExplorationState() = 0;

  // Returns the same data as `Next` method. However, it does not generate the
  // round by inserting new tasks in it, but schedules the threads accoding to
  // the strategy policy with previously genereated and saved round (used for
  // round replaying functionality)
  virtual std::optional<TaskWithMetaData> NextSchedule() = 0;

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
  // The round should be executed via Scheduler::ExploreRound instead of a
  // generated round.
  virtual void SetCustomRound(CustomRound& custom_round) = 0;

  // Returns the number of non-removed tasks
  virtual int GetValidTasksCount() const = 0;

  // Returns the total number of tasks (including removed)
  virtual int GetTotalTasksCount() const = 0;

  // Returns the number of threads
  virtual int GetThreadsCount() const = 0;

  // Called when the finished task must be reported to the verifier
  // (Strategy is a pure interface, the templated subclass
  // BaseStrategyWithThreads knows about the Verifier and will delegate to that)
  virtual void OnVerifierTaskFinish(Task& task, size_t thread_id) = 0;

  // Checks whether starting this already-created task is semantically legal
  // in the current replay/exploration state.
  virtual bool VerifyTaskStart(Task& task, size_t thread_id,
                               const ltest::StartContext& ctx) = 0;

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

  // id of next generated task
  int new_task_id = 0;
  // stores task ids that are removed during the round minimization
  std::unordered_set<int> removed_tasks;
  // when generated round is explored this vector stores indexes of tasks
  // that will be invoked next in each thread
  std::vector<int> round_schedule;
  bool allow_new_tasks_ = true;
};

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct BaseStrategyWithThreads : public Strategy {
  using TargetFactory = std::function<std::unique_ptr<TargetObj>()>;

  BaseStrategyWithThreads(size_t threads_count,
                          std::vector<TaskBuilder> constructors,
                          TargetFactory target_factory,
                          size_t seed = 0)
      : state(target_factory()),
        threads_count(threads_count),
        constructors(std::move(constructors)),
        target_factory(std::move(target_factory)) {
    round_schedule.resize(threads_count, -1);

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
      rng = std::mt19937(static_cast<std::mt19937::result_type>(seed));
    }

    ltest::verifier_hooks::OnRoundStart(sched_checker, threads_count);
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
        constructors.begin(), constructors.end(), [&](const TaskBuilder& b) {
          return b.GetName() == method_name;
        });
  }

  std::optional<int> AppendTaskForReplay(std::string_view method_name,
                                         size_t thread_id) override {
    if (thread_id >= threads.size()) {
      return std::nullopt;
    }

    auto constructor_it =
        std::find_if(constructors.begin(), constructors.end(),
                     [&](const TaskBuilder& b) {
                       return b.GetName() == method_name;
                     });

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

    if (!thread.back()->IsReturned()) {
      FinishTaskDuringTermination(thread.back());
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
      return sched_checker.GetDeadlockProgressMethods(
          std::string(wait_method));
    } else {
      return {};
    }
  }

  void ResetCurrentRound() override {
    this->SetAllowNewTasks(true);
    AbortForRoundReset();
    std::fill(round_schedule.begin(), round_schedule.end(), -1);

    state = target_factory();

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
    this->SetAllowNewTasks(false);
    this->new_task_id = 0;
    this->removed_tasks.clear();
    AbortForRoundReset();

    const size_t custom_threads_count = custom_round.threads.size();
    this->threads.clear();
    this->threads.resize(custom_threads_count);
    this->round_schedule.assign(custom_threads_count, -1);
    this->state = target_factory();

    ltest::verifier_hooks::OnRoundStart(sched_checker,
                                        custom_threads_count);

    for (size_t current_thread = 0; current_thread < custom_threads_count;
         ++current_thread) {
      auto& builders = custom_round.threads[current_thread];
      StableVector<Task> thread_tasks;
      for (auto& task_builder : builders) {
        auto task = task_builder.Build(state.get(), current_thread,
                                       new_task_id++);
        thread_tasks.emplace_back(std::move(task));
      }
      this->threads[current_thread] = std::move(thread_tasks);
    }
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
           sched_checker.Verify(std::string(task->GetName()), thread_id);
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
                    verifier.VerifyExisting(existing_task,
                                            existing_thread_id);
                  }) {
      return sched_checker.VerifyExisting(task, thread_id);
    } else {
      return sched_checker.Verify(std::string(task->GetName()), thread_id);
    }
  }

  std::optional<TaskWithMetaData> Next() override {
    return NextVerifiedFor(NextThreadId());
  }

  std::optional<TaskWithMetaData> NextVerifiedFor(
      std::optional<size_t> opt_thread_index) {
    if (!opt_thread_index.has_value()) {
      return std::nullopt;
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

        if (ltest::verifier_hooks::VerifyStart(sched_checker, constructor.GetName(), thread_index,
                                            ctx) &&
            this->sched_checker.Verify(constructor.GetName(), thread_index)) {
          verified_constructor = i;
          break;
        }
      }

      if (verified_constructor == static_cast<size_t>(-1)) {
        return std::nullopt;
      }

      const TaskBuilder& chosen = this->constructors[verified_constructor];
      const std::string& method_name = chosen.GetName();

      Task task =
          chosen.Build(this->state.get(), thread_index, this->new_task_id++);
      ltest::verifier_hooks::OnTaskStarted(sched_checker, method_name, thread_index,
                                        task->GetId());

      threads[thread_index].emplace_back(std::move(task));

    }

    return TaskWithMetaData{threads[thread_index].back(), is_new, thread_index};
  }

  // Terminates all running tasks.
  // We do it in a dangerous way: in random order.
  // Actually, we assume obstruction free here.
  void TerminateTasks() override {
    auto result = DrainTasks();
    if (result.status == CleanupResult::DRAINED) {
      return;
    }

    AbortTasks(std::move(result.original_thread_sizes));
  }

  void AbortTasksForFailure() override {
    AbortForRoundReset();
  }

  // Lightweight reset strictly for ExploreRound loops.
  // Keeps task objects & scheduler bookkeeping intact, resets spec state & block queues.
  void ResetExplorationState() override {
    AbortForRoundReset();

    std::fill(round_schedule.begin(), round_schedule.end(), -1);
    this->SetAllowNewTasks(true);
    ltest::verifier_hooks::OnRoundStart(sched_checker, threads_count);

    for (auto& thread : threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        if (!IsTaskRemoved(thread[i]->GetId())) {
          thread[i] = thread[i]->Restart(this->state.get());
        }
      }
    }
  }
 protected:
  enum class CleanupResult {
    DRAINED,
    STUCK,
  };

  struct CleanupRunResult {
    CleanupResult status;
    std::vector<size_t> original_thread_sizes;
  };

  bool IsCleanupTask(size_t thread_id,
                   size_t task_index,
                   const std::vector<size_t>& original_thread_sizes) const {
    return task_index >= original_thread_sizes[thread_id];
  }

  bool AppendReleaseTaskIfAny(size_t thread_index) {
    std::optional<std::string> releaseTask =
        this->sched_checker.ReleaseTask(thread_index);

    if (!releaseTask) {
      return false;
    }

    auto constructor_it =
        std::find_if(constructors.begin(), constructors.end(),
                     [&](const TaskBuilder& b) {
                       return b.GetName() == *releaseTask;
                     });

    if (constructor_it == constructors.end()) {
      return false;
    }

    if (!this->sched_checker.Verify(*releaseTask, thread_index)) {
      return false;
    }

    auto task =
        constructor_it->Build(this->state.get(), thread_index, this->new_task_id++);

    ltest::verifier_hooks::OnTaskStarted(
        sched_checker, std::string(task->GetName()), thread_index, task->GetId());
    this->threads[thread_index].emplace_back(task);
    return true;
  }

  void FinishTaskDuringTermination(Task& task) {
    if (task->IsReturned()) {
      return;
    }

    // Let the fiber reach its own return path whenever possible. Merely
    // marking an unfinished boost::context fiber as returned can make its
    // destructor throw boost::context::detail::forced_unwind.
    const bool old_terminating = ltest_round_terminating;
    ltest_round_terminating = true;
    task->TryTerminate();
    ltest_round_terminating = old_terminating;

    if (!task->IsReturned()) {
      task->MarkFinishedDuringTermination();
    }
  }

  void FinalizeCleanup(const std::vector<size_t>& original_thread_sizes) {
    // Some awaiters explicitly require cleanup while the old target is still
    // alive (for example Folly coroutine wrappers that unregister waiters
    // directly from the target object). This must apply to all completed
    // tasks, not only termination-marked ones: round reset may destroy the
    // old target after a fully successful round as well.
    for (auto& thread : this->threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        if (thread[i]->CleanupBeforeTargetDestroy()) {
          thread[i]->RunDeferredCleanup();
        }
      }
    }

    // Some coroutine targets (for example libcoro queue/ring_buffer) wake
    // registered awaiters from their destructor. Keep awaiter/task objects
    // alive while destroying the old target, then release deferred coroutine
    // handles and keepalive objects.
    state = target_factory();

    for (auto& thread : this->threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        if (!thread[i]->CleanupBeforeTargetDestroy()) {
          thread[i]->RunDeferredCleanup();
        }
      }
    }

    for (size_t tid = 0; tid < this->threads.size(); ++tid) {
      auto& thread = this->threads[tid];
      while (thread.size() > original_thread_sizes[tid]) {
        thread.pop_back();
      }
    }

    block_manager.queues.clear();
    ltest_round_terminating = false;
  }

  void CutOffThreadUserSuffix(
    size_t thread_index,
    std::vector<size_t>& task_indexes,
    const std::vector<size_t>& original_thread_sizes) {
    auto& thread = this->threads[thread_index];
    auto& task_index = task_indexes[thread_index];

    const size_t limit =
        std::min(original_thread_sizes[thread_index], thread.size());

    for (size_t i = task_index; i < limit; ++i) {
      if (!thread[i]->IsReturned()) {
        FinishTaskDuringTermination(thread[i]);
      }
    }

    task_index = limit;
  }

    CleanupRunResult DrainTasks() {
    ltest_round_terminating = false;

    std::vector<size_t> original_thread_sizes;
    original_thread_sizes.reserve(this->threads.size());
    for (auto& thread : this->threads) {
      original_thread_sizes.push_back(thread.size());
    }

    auto& round_schedule = this->round_schedule;
    assert(round_schedule.size() == this->threads.size() &&
           "sizes expected to be the same");
    round_schedule.assign(round_schedule.size(), -1);

    std::vector<size_t> task_indexes(this->threads.size(), 0);
    std::vector<bool> thread_cutoff(this->threads.size(), false);

    while (true) {
      bool made_progress = false;
      bool has_unfinished = false;

      // ----------------------------
      // Pass 0: normalize indexes / apply thread cutoff to user suffix
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (thread_cutoff[thread_index] &&
            task_index < std::min(original_thread_sizes[thread_index],
                                  thread.size())) {
          CutOffThreadUserSuffix(thread_index, task_indexes,
                                 original_thread_sizes);
        }

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }
      }

      // ----------------------------
      // Pass 1: append cleanup release tasks to idle threads
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        if (task_index < thread.size()) {
          has_unfinished = true;
          continue;
        }

        if (AppendReleaseTaskIfAny(thread_index)) {
          has_unfinished = true;
        }
      }

      // ----------------------------
      // Pass 2: run ALL cleanup tasks to completion first
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (task_index >= thread.size()) {
          continue;
        }

        if (!IsCleanupTask(thread_index, task_index, original_thread_sizes)) {
          continue;
        }

        auto& task = thread[task_index];

        // Hidden cleanup task must have strict priority and should not be
        // interleaved with ordinary user tasks.
        while (!task->IsReturned() && !task->IsBlocked()) {
          task->Resume();
          made_progress = true;
        }

        if (task->IsReturned()) {
          OnVerifierTaskFinish(task, thread_index);
        } else if (task->IsBlocked()) {
          return {CleanupResult::STUCK, original_thread_sizes};
        }
      }

      // Cleanup tasks may wake blocked user tasks. Do not restart the cleanup
      // loop here, otherwise ReleaseTask() can keep issuing producers while the
      // awakened task never gets a chance to resume and update verifier state.

      // ----------------------------
      // Pass 3: run ordinary user tasks, but only if they are still legal
      // in the current post-cleanup state.
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (task_index < thread.size()) {
          has_unfinished = true;
        } else {
          continue;
        }

        // Cleanup tasks are handled only in Pass 2.
        if (IsCleanupTask(thread_index, task_index, original_thread_sizes)) {
          continue;
        }

        if (thread_cutoff[thread_index]) {
          continue;
        }

	        auto& task = thread[task_index];

	        // IMPORTANT:
	        // A task that was legal in the original execution may become illegal
        // after cleanup release chain changed the semantic state.
        if (!task->IsReturned() &&
            !this->VerifyExistingTask(task, thread_index)) {
          thread_cutoff[thread_index] = true;
          CutOffThreadUserSuffix(thread_index, task_indexes,
                                 original_thread_sizes);
          made_progress = true;
	          continue;
	        }

	        OnVerifierTaskStart(task, thread_index);

	        while (!task->IsReturned() && !task->IsBlocked()) {
          task->Resume();
          made_progress = true;
        }

        if (task->IsReturned()) {
          OnVerifierTaskFinish(task, thread_index);
        }
      }

      // ----------------------------
      // Completion / stuck
      // ----------------------------
      // Recompute whether anything semantically unfinished still remains.
      bool semantically_unfinished = false;
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (task_index >= thread.size()) {
          continue;
        }

        // If remaining task is a user task in a cut-off thread, ignore it:
        // we already declared this suffix semantically unreachable and marked it.
        if (thread_cutoff[thread_index] &&
            !IsCleanupTask(thread_index, task_index, original_thread_sizes)) {
          continue;
        }

        semantically_unfinished = true;
        break;
      }

      if (!semantically_unfinished) {
        bool appended_release_task = false;
        for (size_t thread_index = 0; thread_index < this->threads.size();
             ++thread_index) {
          auto& thread = this->threads[thread_index];
          auto task_index = task_indexes[thread_index];

          while (task_index < thread.size() && thread[task_index]->IsReturned()) {
            task_index++;
          }

          if (task_index >= thread.size() &&
              AppendReleaseTaskIfAny(thread_index)) {
            appended_release_task = true;
          }
        }

        if (appended_release_task) {
          continue;
        }

        FinalizeCleanup(original_thread_sizes);
        return {CleanupResult::DRAINED, std::move(original_thread_sizes)};
      }

      if (!made_progress) {
        return {CleanupResult::STUCK, original_thread_sizes};
      }
    }
  }

    void AbortTasks(std::vector<size_t> original_thread_sizes = {}) {
    ltest_round_terminating = true;

    if (original_thread_sizes.empty()) {
      original_thread_sizes.reserve(this->threads.size());
      for (auto& thread : this->threads) {
        original_thread_sizes.push_back(thread.size());
      }
    }

    auto& round_schedule = this->round_schedule;
    assert(round_schedule.size() == this->threads.size() &&
           "sizes expected to be the same");
    round_schedule.assign(round_schedule.size(), -1);

    std::vector<size_t> task_indexes(this->threads.size(), 0);
    std::vector<bool> thread_cutoff(this->threads.size(), false);

    bool forced_unblock_phase = false;

    while (true) {
      bool made_progress = false;

      // ----------------------------
      // Pass 0: normalize indexes / apply thread cutoff to user suffix
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (thread_cutoff[thread_index] &&
            task_index < std::min(original_thread_sizes[thread_index],
                                  thread.size())) {
          CutOffThreadUserSuffix(thread_index, task_indexes,
                                 original_thread_sizes);
        }

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }
      }

      // ----------------------------
      // Pass 1: append cleanup release tasks to idle threads
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        if (task_index < thread.size()) {
          continue;
        }

        AppendReleaseTaskIfAny(thread_index);
      }

      // ----------------------------
      // Pass 2: run ALL cleanup tasks to completion first
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (task_index >= thread.size()) {
          continue;
        }

        if (!IsCleanupTask(thread_index, task_index, original_thread_sizes)) {
          continue;
        }

        auto& task = thread[task_index];

        while (!task->IsReturned() && !task->IsBlocked()) {
          task->Resume();
          made_progress = true;
        }

        if (task->IsReturned()) {
          OnVerifierTaskFinish(task, thread_index);
        }
      }

      // Cleanup tasks may wake blocked user tasks; let Pass 3 observe that
      // progress before asking the verifier for another cleanup producer.

      // ----------------------------
      // Pass 3: run ordinary user tasks if still semantically legal
      // ----------------------------
      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (task_index >= thread.size()) {
          continue;
        }

        if (IsCleanupTask(thread_index, task_index, original_thread_sizes)) {
          continue;
        }

        if (thread_cutoff[thread_index]) {
          continue;
        }

	        auto& task = thread[task_index];

	        if (!task->IsReturned() &&
	            !this->VerifyExistingTask(task, thread_index)) {
          thread_cutoff[thread_index] = true;
          CutOffThreadUserSuffix(thread_index, task_indexes,
                                 original_thread_sizes);
          made_progress = true;
	          continue;
	        }

	        OnVerifierTaskStart(task, thread_index);

	        while (!task->IsReturned() && !task->IsBlocked()) {
          task->Resume();
          made_progress = true;
        }

        if (task->IsReturned()) {
          OnVerifierTaskFinish(task, thread_index);

          if (task->FinishedDuringTermination()) {
            thread_cutoff[thread_index] = true;
            CutOffThreadUserSuffix(thread_index, task_indexes,
                                   original_thread_sizes);
          }

        }
      }

      if (!made_progress) {
        if (!forced_unblock_phase) {
          block_manager.queues.clear();
          forced_unblock_phase = true;
          continue;
        }
        break;
      }
    }

    // Mark any remaining unfinished tasks as finished during termination.
    for (auto& thread : this->threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        if (!thread[i]->IsReturned()) {
          FinishTaskDuringTermination(thread[i]);
        }
      }
    }

    FinalizeCleanup(original_thread_sizes);
  }

  void AbortForRoundReset() {
    ltest_round_terminating = true;

    std::vector<size_t> original_thread_sizes;
    original_thread_sizes.reserve(this->threads.size());
    for (auto& thread : this->threads) {
      original_thread_sizes.push_back(thread.size());
    }

    for (auto& thread : this->threads) {
      for (size_t i = 0; i < thread.size(); ++i) {
        if (!thread[i]->IsReturned()) {
          FinishTaskDuringTermination(thread[i]);
        }
      }
    }

    FinalizeCleanup(original_thread_sizes);
  }



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

  Verifier sched_checker{};
  std::unique_ptr<TargetObj> state;
  // Strategy struct is the owner of all tasks, and all
  // references can't be invalidated before the end of the round,
  // so we have to contains all tasks in queues(queue doesn't invalidate the
  // references)
  size_t threads_count;
  std::vector<StableVector<Task>> threads;
  std::vector<TaskBuilder> constructors;
  TargetFactory target_factory;
  std::uniform_int_distribution<std::mt19937::result_type>
      constructors_distribution;
  std::mt19937 rng;
};

#include "minimization.h"
#include "minimization_smart.h"

static inline std::pair<bool, bool> GetUnfinishedAndRunnable(
    const Strategy& strategy,
    const std::unordered_set<int>* allowed_ids) {
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

static inline ltest::StartContext BuildReplayStartContext(
    const Strategy& strategy,
    const std::unordered_set<int>& started_ids) {
  ltest::StartContext ctx{};
  const auto& threads = strategy.GetTasks();
  ctx.threads = threads.size();

  for (size_t tid = 0; tid < threads.size(); ++tid) {
    bool has_active_started_task = false;

    for (size_t i = 0; i < threads[tid].size(); ++i) {
      const auto& task = threads[tid][i];
      int id = task->GetId();

      // Only tasks that have already semantically started in this replay/explore
      // contribute to the context.
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
static inline bool IsReportableDeadlockHistory(
    const std::vector<Event>& seq) {
  // An empty/no-op replay can mean that no legal operation was generated,
  // but it is not a user-visible deadlock.
  return RoundMinimizorT<Event>::HasAnyStartedOp(seq) &&
         MinBadTraits<Event>::HasPendingVisibleOp(seq);
}

struct TLAThread {
  size_t id;
  StableVector<Task> tasks;
};

struct TLAFrame {
  Task* task{};
  bool is_new{};
};

static inline void InitTLAThreads(StableVector<TLAThread>& threads,
                                  size_t threads_count) {
  for (size_t i = 0; i < threads_count; ++i) {
    threads.emplace_back(TLAThread{
        .id = i,
        .tasks = StableVector<Task>{},
    });
  }
}

template <class Threads, class Fn>
static inline void ForEachTLATask(Threads& threads, Fn&& fn) {
  for (size_t tid = 0; tid < threads.size(); ++tid) {
    auto& thread = threads[tid];
    for (size_t task_index = 0; task_index < thread.tasks.size();
         ++task_index) {
      fn(tid, task_index, thread.tasks[task_index]);
    }
  }
}

template <class Threads, class Fn>
static inline void ForEachTLATask(const Threads& threads, Fn&& fn) {
  for (size_t tid = 0; tid < threads.size(); ++tid) {
    const auto& thread = threads[tid];
    for (size_t task_index = 0; task_index < thread.tasks.size();
         ++task_index) {
      fn(tid, task_index, thread.tasks[task_index]);
    }
  }
}

template <class Threads>
static inline bool HasUnfinishedTLATasks(const Threads& threads) {
  bool has_unfinished = false;
  ForEachTLATask(threads, [&](size_t, size_t, const Task& task) {
    has_unfinished = has_unfinished || !task->IsReturned();
  });
  return has_unfinished;
}

template <class Threads>
static inline size_t FindTLATaskThread(const Threads& threads,
                                       const Task& needle) {
  for (size_t tid = 0; tid < threads.size(); ++tid) {
    for (size_t task_index = 0; task_index < threads[tid].tasks.size();
         ++task_index) {
      const auto& task = threads[tid].tasks[task_index];
      if (task.get() == needle.get()) {
        return tid;
      }
    }
  }
  assert(false && "task not found in TLA scheduler");
  return 0;
}

template <class Threads>
static inline ltest::StartContext BuildTLAStartContext(
    const Threads& threads) {
  ltest::StartContext ctx{};
  ctx.threads = threads.size();

  for (size_t tid = 0; tid < threads.size(); ++tid) {
    const auto& thread = threads[tid];
    bool has_active_task = false;
    if (!thread.tasks.empty()) {
      const auto& task = thread.tasks.back();
      if (!task->IsReturned()) {
        has_active_task = true;
        std::string name = std::string(task->GetName());
        ctx.active_by_method[name]++;
        if (task->IsBlocked()) {
          ctx.blocked_by_method[name]++;
        }
      }
    }

    if (!has_active_task) {
      ctx.free_threads++;
    }
  }

  return ctx;
}

static inline void AppendDualStartEvent(
    std::vector<DualHistoryEvent>& seq, Task& task, bool is_new,
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
      drained.push_back(DrainedDualEventRecord{
          e.seqno, task, thread_id, std::move(e)});
    }
  }
}

static inline void AppendCollectedDrainedDualEvents(
    std::vector<DualHistoryEvent>& seq,
    std::vector<DrainedDualEventRecord>& drained) {
  if (drained.empty()) {
    return;
  }

  std::sort(drained.begin(), drained.end(),
            [](const DrainedDualEventRecord& lhs,
               const DrainedDualEventRecord& rhs) {
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
        seq.emplace_back(FollowUpResponse(rec.task, rec.event.result,
                                          rec.thread_id));
        break;
    }
  }
}

static inline void AppendDrainedDualEvents(
    std::vector<DualHistoryEvent>& seq, Strategy& strategy) {
  std::vector<DrainedDualEventRecord> drained;

  const auto& threads = strategy.GetTasks();
  for (size_t thread_id = 0; thread_id < threads.size(); ++thread_id) {
    CollectDrainedDualEvents(drained, threads[thread_id],
                             static_cast<int>(thread_id));
  }

  AppendCollectedDrainedDualEvents(seq, drained);
}

template <class Threads>
static inline void AppendDrainedDualEvents(
    std::vector<DualHistoryEvent>& seq, const Threads& threads) {
  std::vector<DrainedDualEventRecord> drained;

  for (size_t thread_id = 0; thread_id < threads.size(); ++thread_id) {
    CollectDrainedDualEvents(drained, threads[thread_id].tasks,
                             static_cast<int>(thread_id));
  }

  AppendCollectedDrainedDualEvents(seq, drained);
}

static inline void DiscardDrainedDualEvents(const Task& task) {
  if (task->IsDual() && task->HasDualEvents()) {
    (void)task->DrainDualEvents();
  }
}

template <class Threads>
static inline void DiscardDrainedDualEvents(const Threads& threads) {
  ForEachTLATask(threads, [](size_t, size_t, const Task& task) {
    DiscardDrainedDualEvents(task);
  });
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
        deadlock_policy(deadlock_policy)
    {}

  // Run returns full unliniarizable history if such a history is found. Full
  // history is a history with all events, where each element in the vector is a
  // Resume operation on the corresponding task
  Scheduler::Result Run() override {
    for (size_t round = 0; round < custom_rounds.size() + max_rounds;
         ++round) {
      Result histories;
      if (round < custom_rounds.size()) {
        log() << "\nexplore custom round: " << round << "\n\n";
        strategy.SetCustomRound(custom_rounds[round]);
        histories = ExploreRound(
            static_cast<int>(std::max<size_t>(1, exploration_runs)));
      } else {
        const size_t generated_round = round - custom_rounds.size();
        log() << "\nrun round: " << generated_round << "\n\n";
        histories = RunRound();
      }

      if (histories.has_value()) {
        auto& [full_history, sequential_history, reason] = histories.value();

        if (should_minimize_history) {
          log() << "Full nonlinear scenario: \n";
          pretty_printer.PrettyPrint(sequential_history, log());

          if (histories->reason ==
              NonLinearizableHistory::Reason::DEADLOCK) {
            log() << "Skipping replay minimization for deadlock.\n";
          } else {
            log() << "Minimizing same interleaving...\n";
            Minimize(histories.value(), SameInterleavingMinimizor());
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(sequential_history, log());

            log() << "Minimizing with rescheduling (exploration runs: "
                  << exploration_runs << ")...\n";
            Minimize(histories.value(),
                     StrategyExplorationMinimizor(exploration_runs));
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(sequential_history, log());

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

 protected:
  // Runs a round with some interleaving while generating it
  Result RunRound() override {
    // History of invoke and response events which is required for the checker
    SeqHistory sequential_history;
    // Full history of the current execution in the Run function
    FullHistory full_history;

    bool deadlock_detected{false};

    size_t started_tasks = 0;
    size_t finished_tasks = 0;  // only for statistics / debug prints

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
      if (!t.has_value()) {
        deadlock_detected = true;
        break;
      }
      auto [next_task, is_new, thread_id] = t.value();

      next_task->clearWakeupCondition();

      // fill the sequential history
      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
        ++started_tasks;
      }
      full_history.emplace_back(next_task);

      next_task->Resume();
      if (next_task->IsReturned()) {
        ++finished_tasks;
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    pretty_printer.PrettyPrint(sequential_history, log());

    if (deadlock_detected) {
      if (deadlock_policy != DeadlockPolicy::Fail) {
        if (!checker.Check(sequential_history)) {
          return NonLinearizableHistory(
              full_history, sequential_history,
              NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
        }

        if (deadlock_policy == DeadlockPolicy::Check) {
          return std::nullopt;
        }

        // TODO(bitree2004):: only for PCT, Random. TLA, RR
        const int k = static_cast<int>(exploration_runs);
        auto alt = ExploreRound(k);

        if (alt.has_value()) {
          return alt;
        }

        return std::nullopt;
      }
      if (!IsReportableDeadlockHistory(sequential_history)) {
        return std::nullopt;
      }
      return NonLinearizableHistory(full_history, sequential_history,
                                    NonLinearizableHistory::Reason::DEADLOCK);
    }

    if (!checker.Check(sequential_history)) {
      return NonLinearizableHistory(
          full_history, sequential_history,
          NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
    }

    return std::nullopt;
  }
  // Runs different interleavings of the current round
  Result ExploreRound(int runs) override {
  for (int i = 0; i < runs; ++i) {
    strategy.ResetCurrentRound();
    SeqHistory sequential_history;
    FullHistory full_history;

    bool deadlock_detected{false};

    for (int tasks_to_run = strategy.GetValidTasksCount();
         tasks_to_run > 0;) {
      auto t = strategy.NextSchedule();
      if (!t.has_value()) {
        deadlock_detected = true;
        break;
      }
      auto [next_task, is_new, thread_id] = t.value();

      // Existing task may no longer be legal in the current replay/exploration
      // state (for example, old unlock after a lock did not complete normally).
      if (!next_task->IsReturned() &&
          !strategy.VerifyExistingTask(next_task,
                                       static_cast<size_t>(thread_id))) {
        deadlock_detected = true;
        break;
      }

      if (is_new) {
        strategy.OnVerifierTaskStart(next_task, thread_id);
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      full_history.emplace_back(next_task);

      next_task->Resume();
      if (next_task->IsReturned()) {
        tasks_to_run--;
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    if (deadlock_detected) {
      if (deadlock_policy != DeadlockPolicy::Fail) {
        // benign blocking: just try next exploration run
        if (checker.Check(sequential_history)) {
          continue;
        }
        // bad prefix -> report as non-linearizable
        return NonLinearizableHistory(
            full_history, sequential_history,
            NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
      }

      if (!IsReportableDeadlockHistory(sequential_history)) {
        continue;
      }

      return NonLinearizableHistory(
          full_history, sequential_history,
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

    // Replays current round with specified interleaving.
  //
  // Replay works over a precomputed ordering of already-generated tasks.
  // If some existing task is no longer semantically legal in the current
  // replay state, we cut off the rest of that thread in this replay execution.
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

    // If some task in a thread becomes illegal in this replay execution,
    // skip the rest of that thread's user tasks.
    std::vector<bool> thread_cutoff(strategy.GetThreadsCount(), false);

    for (size_t step = 0; step < tasks_ordering.size(); ++step) {
      int next_task_id = tasks_ordering[step];

      auto task_info = strategy.GetTask(next_task_id);

      if (!task_info.has_value()) {
        std::cerr << "No task with id " << next_task_id
                  << " exists in round\n";
        throw std::runtime_error("Invalid task id");
      }

      auto [next_task, thread_id] = task_info.value();

      if (thread_cutoff[thread_id]) {
        continue;
      }

      // Existing task may no longer be legal in this replay state.
      if (!next_task->IsReturned() &&
          !strategy.VerifyExistingTask(next_task,
                                       static_cast<size_t>(thread_id))) {
        thread_cutoff[thread_id] = true;
        continue;
      }

      bool is_new = started_tasks.insert(next_task_id).second;
      next_task->clearWakeupCondition();

      if (is_new) {
        strategy.OnVerifierTaskStart(next_task, thread_id);
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      full_history.emplace_back(next_task);

      if (next_task->IsReturned()) continue;

      const bool is_last = (last_pos[next_task_id] == step);

      if (mode == ReplayMode::CompleteOnLast && is_last) {
        next_task->Terminate();
      } else {
        next_task->Resume();
      }

      if (next_task->IsReturned()) {
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        if (next_task->FinishedDuringTermination()) {
          thread_cutoff[thread_id] = true;
        }

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    // Deadlock-friendly replay: detect "stuck" after ordering without forcing completion.
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

// TLAScheduler generates all executions satisfying some conditions.
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct TLAScheduler : Scheduler {
  using TargetFactory = std::function<std::unique_ptr<TargetObj>()>;

  TLAScheduler(size_t max_tasks, size_t max_rounds, size_t threads_count,
               size_t max_switches, size_t max_depth,
               std::vector<TaskBuilder> constructors, ModelChecker& checker,
               PrettyPrinter& pretty_printer, std::function<void()> cancel_func,
               TargetFactory target_factory)
      : max_tasks{max_tasks},
        max_rounds{max_rounds},
        max_switches{max_switches},
        constructors{std::move(constructors)},
        checker{checker},
        pretty_printer{pretty_printer},
        max_depth(max_depth),
        cancel(cancel_func),
        target_factory(std::move(target_factory)) {
    InitTLAThreads(threads, threads_count);
    state = this->target_factory();
  };

  Scheduler::Result Run() override {
    auto [_, res] = RunStep(0, 0);
    return res;
  }

  ~TLAScheduler() { TerminateTasks(); }

 private:
  using Thread = TLAThread;

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
  using Frame = TLAFrame;

  // Terminates all running tasks.
  // We do it in a dangerous way: in random order.
  // Actually, we assume obstruction free here.
  // cancel() func takes care for graceful shutdown
  void TerminateTasks() {
    cancel();
    ForEachTLATask(threads, [](size_t, size_t, Task& task) {
      if (!task->IsReturned()) {
        task->Terminate();
      }
    });
  }

  // Replays all actions from 0 to the step_end.
  void Replay(size_t step_end) {
    // Firstly, terminate all running tasks.
    TerminateTasks();
    // In histories we store references, so there's no need to update it.
    state = target_factory();
    for (size_t step = 0; step < step_end; ++step) {
      auto& frame = frames[step];
      auto task = frame.task;
      assert(task);
      if (frame.is_new) {
        // It was a new task.
        // So restart it from the beginning with the same args.
        *task = (*task)->Restart(state.get());
      } else {
        // It was a not new task, hence, we recreated in early.
      }
      (*task)->Resume();
    }
    coroutine_status.reset();
  }

  void UpdateFullHistory(size_t thread_id, Task& task, bool is_new) {
    if (coroutine_status.has_value()) {
      if (is_new) {
        assert(coroutine_status->has_started);
        full_history.emplace_back(thread_id, task);
      }
      // To prevent cases like this
      //  +--------+--------+
      //  |   T1   |   T2   |
      //  +--------+--------+
      //  |        | Recv   |
      //  | Send   |        |
      //  |        | >read  |
      //  | >flush |        |
      //  +--------+--------+
      full_history.emplace_back(thread_id, coroutine_status.value());
      coroutine_status.reset();
    } else {
      full_history.emplace_back(thread_id, task);
    }
  }
  // Resumes choosed task.
  // If task is finished and finished tasks == max_tasks, stops.
  std::tuple<bool, typename Scheduler::Result> ResumeTask(
      Frame& frame, size_t step, size_t switches, Thread& thread, bool is_new) {
    auto thread_id = thread.id;
    size_t previous_thread_id = thread_id_history.empty()
                                    ? std::numeric_limits<size_t>::max()
                                    : thread_id_history.back();
    size_t nxt_switches = switches;
    if (!is_new) {
      if (thread_id != previous_thread_id) {
        ++nxt_switches;
      }
      if (nxt_switches > max_switches) {
        // The limit of switches is achieved.
        // So, do not resume task.
        return {false, {}};
      }
    }
    auto& task = thread.tasks.back();
    frame.task = &task;

    thread_id_history.push_back(thread_id);
    if (is_new) {
      sequential_history.emplace_back(Invoke(task, thread_id));
    }

    assert(!task->IsBlocked());
    task->Resume();
    UpdateFullHistory(thread_id, task, is_new);
    bool is_finished = task->IsReturned();
    if (is_finished) {
      finished_tasks++;
      verifier.OnFinished(task, thread.id);
      auto result = task->GetRetVal();
      sequential_history.emplace_back(Response(task, result, thread_id));
    }

    bool stop = finished_tasks == max_tasks;
    if (!stop) {
      // Run recursive step.
      auto [is_over, res] = RunStep(step + 1, nxt_switches);
      if (is_over || res.has_value()) {
        return {is_over, res};
      }
    } else {
      log() << "\nrun round: " << finished_rounds << "\n\n";
      pretty_printer.PrettyPrint(full_history, log());
      log() << "===============================================\n\n";
      log().flush();
      // Stop, check if the the generated history is linearizable.
      ++finished_rounds;
      if (!checker.Check(sequential_history)) {
        return {false,
                NonLinearizableHistory(
                    FullHistory{}, sequential_history,
                    NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY)};
      }
      if (finished_rounds == max_rounds) {
        // It was the last round.
        return {true, {}};
      }
    }

    thread_id_history.pop_back();
    // Removing combination of start of task + coroutine start
    if (full_history.back().second.index() == 1) {
      auto& cor = std::get<1>(full_history.back().second);
      auto& prev = full_history[full_history.size() - 2];
      int thread = full_history.back().first;
      auto first_ind =
          std::find_if(full_history.begin(), --full_history.end(),
                       [&thread](auto& a) { return a.first == thread; });
      if (cor.has_started &&
          std::distance(full_history.begin(), first_ind) ==
              full_history.size() - 2 &&
          prev.second.index() == 0) {
        full_history.pop_back();
      }
    }
    full_history.pop_back();
    if (is_finished) {
      --finished_tasks;
      // resp.
      sequential_history.pop_back();
    }
    if (is_new) {
      // inv.
      --started_tasks;
      sequential_history.pop_back();
    }

    return {false, {}};
  }

  std::tuple<bool, typename Scheduler::Result> RunStep(size_t step,
                                                       size_t switches) {
    // Push frame to the stack.
    frames.emplace_back(Frame{});
    auto& frame = frames.back();

    bool all_parked = true;
    // Pick next task.
    for (size_t i = 0; i < threads.size(); ++i) {
      auto& thread = threads[i];
      auto& tasks = thread.tasks;
      if (!tasks.empty() && !tasks.back()->IsReturned()) {
        if (tasks.back()->IsBlocked()) {
          continue;
        }
        all_parked = false;
        if (!verifier.Verify(std::string{tasks.back()->GetName()}, i)) {
          continue;
        }
        // Task exists.
        frame.is_new = false;
        auto [is_over, res] = ResumeTask(frame, step, switches, thread, false);
        if (is_over || res.has_value()) {
          return {is_over, res};
        }
        // As we can't return to the past in coroutine, we need to replay all
        // tasks from the beginning.
        Replay(step);
        continue;
      }

      all_parked = false;
      // Choose constructor to create task.
      bool stop = started_tasks == max_tasks;
      if (!stop && threads[i].tasks.size() < max_depth) {
        for (auto cons : constructors) {
          if (!verifier.Verify(cons.GetName(), i)) {
            continue;
          }
          frame.is_new = true;
          auto size_before = tasks.size();
          tasks.emplace_back(
              cons.Build(state.get(), i, static_cast<int>(next_task_id++)));
          started_tasks++;
          auto [is_over, res] = ResumeTask(frame, step, switches, thread, true);
          if (is_over || res.has_value()) {
            return {is_over, res};
          }
          // As we can't return to the past in coroutine, we need to replay all
          // tasks from the beginning.
          Replay(step);
          tasks.pop_back();
          auto size_after = thread.tasks.size();
          assert(size_before == size_after);
        }
      }
    }

    assert(!all_parked && "deadlock");
    frames.pop_back();
    return {false, {}};
  }

  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  size_t max_rounds;
  size_t max_switches;
  size_t max_depth;

  std::vector<TaskBuilder> constructors;
  ModelChecker& checker;

  // Running state.
  size_t started_tasks{};
  size_t finished_tasks{};
  size_t finished_rounds{};
  std::unique_ptr<TargetObj> state;
  std::vector<std::variant<Invoke, Response>> sequential_history;
  FullHistoryWithThreads full_history;
  std::vector<size_t> thread_id_history;
  StableVector<Thread> threads;
  StableVector<Frame> frames;
  Verifier verifier{};
  std::function<void()> cancel;
  TargetFactory target_factory;
  size_t next_task_id{};
};

// DualTLAScheduler enumerates interleavings for dual histories. Unlike the
// ordinary TLA scheduler it bounds started operations, because pending dual
// requests are meaningful history, especially for deadlock checking.
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct DualTLAScheduler : DualScheduler {
  using TargetFactory = std::function<std::unique_ptr<TargetObj>()>;

  DualTLAScheduler(size_t max_tasks, size_t max_rounds, size_t threads_count,
                   size_t max_switches, size_t max_depth,
                   std::vector<TaskBuilder> constructors,
                   DualModelChecker& checker,
                   PrettyPrinter& pretty_printer,
                   DeadlockPolicy deadlock_policy,
                   TargetFactory target_factory)
      : max_tasks{max_tasks},
        max_rounds{max_rounds},
        max_switches{max_switches},
        max_depth(max_depth == 0 ? max_tasks : max_depth),
        constructors{std::move(constructors)},
        checker{checker},
        pretty_printer{pretty_printer},
        deadlock_policy{deadlock_policy},
        target_factory(std::move(target_factory)) {
    InitTLAThreads(threads, threads_count);
    state = this->target_factory();
    ltest::verifier_hooks::OnRoundStart(verifier, threads.size());
  }

  ~DualTLAScheduler() override {
    TerminateTasks();
    DestroyStateAndCleanupTasks();
  }

  DualScheduler::Result Run() override {
    auto [_, res] = RunStep(0, 0);
    return res;
  }

 private:
  using Thread = TLAThread;
  using Frame = TLAFrame;

  void TerminateTasks() {
    const bool old_terminating = ltest_round_terminating;
    ltest_round_terminating = true;

    ForEachTLATask(threads, [](size_t, size_t, Task& task) {
      if (!task->IsReturned()) {
        task->Terminate();
      }
    });
    DiscardDrainedDualEvents(threads);

    block_manager.queues.clear();
    ltest_round_terminating = old_terminating;
  }

  void CleanupTasks() {
    ForEachTLATask(threads, [](size_t, size_t, Task& task) {
      if (task->CleanupBeforeTargetDestroy()) {
        task->RunDeferredCleanup();
      }
    });
  }

  void DestroyStateAndCleanupTasks() {
    const bool old_terminating = ltest_round_terminating;
    ltest_round_terminating = true;

    state.reset();
    CleanupTasks();
    ForEachTLATask(threads, [](size_t, size_t, Task& task) {
      if (!task->CleanupBeforeTargetDestroy()) {
        task->RunDeferredCleanup();
      }
    });
    block_manager.queues.clear();

    ltest_round_terminating = old_terminating;
  }

  void Replay(size_t step_end) {
    TerminateTasks();
    DestroyStateAndCleanupTasks();
    state = target_factory();
    block_manager.queues.clear();
    coroutine_status.reset();
    ltest::verifier_hooks::OnRoundStart(verifier, threads.size());

    for (size_t step = 0; step < step_end; ++step) {
      auto& frame = frames[step];
      auto* task_ptr = frame.task;
      assert(task_ptr);

      if (frame.is_new) {
        *task_ptr = (*task_ptr)->Restart(state.get());
        ltest::verifier_hooks::OnTaskStarted(
            verifier, std::string((*task_ptr)->GetName()),
            FindTLATaskThread(threads, *task_ptr), (*task_ptr)->GetId());
      }

      (*task_ptr)->Resume();
      DiscardDrainedDualEvents(threads);

      if ((*task_ptr)->IsReturned()) {
        verifier.OnFinished(*task_ptr, FindTLATaskThread(threads, *task_ptr));
      }
    }
  }

  bool CompletedStartedPrefix() const {
    return started_tasks == max_tasks && !HasUnfinishedTLATasks(threads);
  }

  std::tuple<bool, DualScheduler::Result> CheckCompletedPrefix() {
    if (!CompletedStartedPrefix()) {
      return {false, std::nullopt};
    }

    log() << "\nrun round: " << finished_rounds << "\n\n";
    pretty_printer.PrettyPrint(sequential_history, log());
    log() << "===============================================\n\n";
    log().flush();

    ++finished_rounds;
    if (!checker.Check(sequential_history)) {
      return {false,
              DualScheduler::NonLinearizableHistory{
                  full_history, sequential_history,
                  DualScheduler::NonLinearizableHistory::Reason::
                      NON_LINEARIZABLE_HISTORY}};
    }

    if (finished_rounds == max_rounds) {
      return {true, std::nullopt};
    }

    return {false, std::nullopt};
  }

  std::tuple<bool, DualScheduler::Result> HandleNoBranch() {
    if (!HasUnfinishedTLATasks(threads)) {
      return {false, std::nullopt};
    }

    if (!checker.Check(sequential_history)) {
      return {false,
              DualScheduler::NonLinearizableHistory{
                  full_history, sequential_history,
                  DualScheduler::NonLinearizableHistory::Reason::
                      NON_LINEARIZABLE_HISTORY}};
    }

    if (deadlock_policy != DeadlockPolicy::Fail ||
        !IsReportableDeadlockHistory(sequential_history)) {
      return {false, std::nullopt};
    }

    return {false,
            DualScheduler::NonLinearizableHistory{
                full_history, sequential_history,
                DualScheduler::NonLinearizableHistory::Reason::DEADLOCK}};
  }

  std::tuple<bool, DualScheduler::Result> ResumeTask(
      Frame& frame, size_t step, size_t switches, Thread& thread,
      bool is_new) {
    const size_t thread_id = thread.id;
    const size_t previous_thread_id =
        thread_id_history.empty() ? std::numeric_limits<size_t>::max()
                                  : thread_id_history.back();
    size_t next_switches = switches;
    if (!is_new && thread_id != previous_thread_id) {
      ++next_switches;
      if (next_switches > max_switches) {
        return {false, std::nullopt};
      }
    }

    auto& task = thread.tasks.back();
    frame.task = &task;

    const size_t seq_size = sequential_history.size();
    const size_t full_size = full_history.size();

    thread_id_history.push_back(thread_id);
    if (is_new) {
      ++started_tasks;
    }

    AppendDualStartEvent(
        sequential_history, task, is_new, static_cast<int>(thread_id));
    full_history.emplace_back(task);

    assert(!task->IsBlocked());
    task->Resume();
    AppendDrainedDualEvents(sequential_history, threads);

    bool finished_now = task->IsReturned();
    if (finished_now) {
      ++finished_tasks;
      verifier.OnFinished(task, thread_id);
      if (!task->IsDual()) {
        sequential_history.emplace_back(
            Response(task, task->GetRetVal(), static_cast<int>(thread_id)));
      }
    }

    auto [is_done, completed_result] = CheckCompletedPrefix();
    if (is_done || completed_result.has_value()) {
      return {is_done, completed_result};
    }

    auto [is_over, nested_result] = RunStep(step + 1, next_switches);
    if (is_over || nested_result.has_value()) {
      return {is_over, nested_result};
    }

    thread_id_history.pop_back();
    if (finished_now) {
      --finished_tasks;
    }
    if (is_new) {
      --started_tasks;
    }

    sequential_history.erase(sequential_history.begin() + seq_size,
                             sequential_history.end());
    full_history.erase(full_history.begin() + full_size, full_history.end());

    return {false, std::nullopt};
  }

  std::tuple<bool, DualScheduler::Result> RunStep(size_t step,
                                                  size_t switches) {
    frames.emplace_back(Frame{});
    auto& frame = frames.back();
    bool explored_branch = false;

    for (size_t tid = 0; tid < threads.size(); ++tid) {
      auto& thread = threads[tid];
      auto& tasks = thread.tasks;
      if (tasks.empty() || tasks.back()->IsReturned() ||
          tasks.back()->IsBlocked()) {
        continue;
      }

      if (!verifier.Verify(std::string(tasks.back()->GetName()), thread.id)) {
        continue;
      }

      frame.is_new = false;
      explored_branch = true;

      auto [is_over, res] = ResumeTask(frame, step, switches, thread, false);
      if (is_over || res.has_value()) {
        return {is_over, res};
      }

      Replay(step);
    }

    if (started_tasks < max_tasks) {
      for (size_t tid = 0; tid < threads.size(); ++tid) {
        auto& thread = threads[tid];
        auto& tasks = thread.tasks;
        if (!tasks.empty() && !tasks.back()->IsReturned()) {
          continue;
        }
        if (tasks.size() >= max_depth) {
          continue;
        }

        for (const auto& cons : constructors) {
          auto ctx = BuildTLAStartContext(threads);
          if (!ltest::verifier_hooks::VerifyStart(
                  verifier, cons.GetName(), thread.id, ctx) ||
              !verifier.Verify(cons.GetName(), thread.id)) {
            continue;
          }

          frame.is_new = true;
          const auto size_before = tasks.size();
          tasks.emplace_back(
              cons.Build(state.get(), thread.id, next_task_id++));
          ltest::verifier_hooks::OnTaskStarted(
              verifier, cons.GetName(), thread.id, tasks.back()->GetId());

          explored_branch = true;
          auto [is_over, res] = ResumeTask(frame, step, switches, thread, true);
          if (is_over || res.has_value()) {
            return {is_over, res};
          }

          Replay(step);
          tasks.pop_back();
          assert(size_before == tasks.size());
        }
      }
    }

    if (!explored_branch) {
      auto [is_over, res] = HandleNoBranch();
      if (is_over || res.has_value()) {
        return {is_over, res};
      }
    }

    frames.pop_back();
    return {false, std::nullopt};
  }

  size_t max_tasks;
  size_t max_rounds;
  size_t max_switches;
  size_t max_depth;
  std::vector<TaskBuilder> constructors;
  DualModelChecker& checker;
  PrettyPrinter& pretty_printer;
  DeadlockPolicy deadlock_policy;

  size_t next_task_id{};
  size_t started_tasks{};
  size_t finished_tasks{};
  size_t finished_rounds{};
  std::unique_ptr<TargetObj> state;
  DualScheduler::SeqHistory sequential_history;
  DualScheduler::FullHistory full_history;
  std::vector<size_t> thread_id_history;
  StableVector<Thread> threads;
  StableVector<Frame> frames;
  Verifier verifier{};
  TargetFactory target_factory;
};

// DualStrategyScheduler: builds dual history by:
// - writing RequestInvoke for dual tasks when task is_new
// - draining task->DrainDualEvents() after each Resume()/Terminate()
// Checker is DualModelChecker.
//
// make it DualSchedulerWithReplay by implementing
// RunRound/ExploreRound/ReplayRound.
template <StrategyTaskVerifier Verifier>
struct DualStrategyScheduler : public DualSchedulerWithReplay {
  DualStrategyScheduler(Strategy& sched_class, DualModelChecker& checker,
                      PrettyPrinter& pretty_printer, size_t max_tasks,
                      size_t max_rounds, bool minimize,
                      size_t exploration_runs,
                      size_t minimization_runs,
                      DeadlockPolicy deadlock_policy = DeadlockPolicy::Fail)
      : strategy(sched_class),
        checker(checker),
        pretty_printer(pretty_printer),
        max_tasks(max_tasks),
        max_rounds(max_rounds),
        should_minimize_history(minimize),
        exploration_runs(exploration_runs),
        minimization_runs(minimization_runs),
        deadlock_policy(deadlock_policy)
        {}

  DualSchedulerWithReplay::Result Run() override {
    for (size_t i = 0; i < max_rounds; ++i) {
      log() << "\nrun round: " << i << "\n\n";
      auto res = RunRound();
      if (res.has_value()) {
        if (should_minimize_history) {
          log() << "Full nonlinear scenario (DUAL):\n";
          pretty_printer.PrettyPrint(res->seq, log());

          if (res->reason ==
              DualSchedulerWithReplay::NonLinearizableHistory::Reason::
                  DEADLOCK) {
            log() << "Skipping replay minimization for deadlock.\n";
          } else {
            log() << "Minimizing same interleaving...\n";
            Minimize(*res, DualSameInterleavingMinimizor{});
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(res->seq, log());

            log() << "Minimizing with rescheduling (exploration runs: "
                  << exploration_runs << ")...\n";
            Minimize(*res, DualStrategyExplorationMinimizor(
                               static_cast<int>(exploration_runs)));
            log() << "Minimized to:\n";
            pretty_printer.PrettyPrint(res->seq, log());

            log() << "Minimizing with smart minimizor (exploration runs: "
                  << exploration_runs
                  << ", minimization runs: " << minimization_runs << ")...\n";
            Minimize(*res, DualSmartMinimizor(static_cast<int>(exploration_runs),
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

 protected:
  // Common helper: start event for new task
  inline void EmitStartEvent(DualSchedulerWithReplay::SeqHistory& seq,
                             Task& task, bool is_new, int thread_id) {
    AppendDualStartEvent(seq, task, is_new, thread_id);
  }

  // Common helper: drain dual events after a step
  inline void DrainDual(Task& task, int thread_id,
                        DualSchedulerWithReplay::SeqHistory& seq) {
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
      const DualSchedulerWithReplay::FullHistory& full) {
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

      auto progress_methods = strategy.GetDeadlockProgressMethods(
          stored_task->GetName());
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

  DualSchedulerWithReplay::Result TryRollbackDeadlock(
      const DualSchedulerWithReplay::FullHistory& full) {
    auto candidate = FindRollbackCandidate(full);
    if (!candidate.has_value()) {
      return std::nullopt;
    }

    const int attempts = std::max(1, static_cast<int>(exploration_runs));
    for (int attempt = 0; attempt < attempts; ++attempt) {
      for (const auto& progress_method : candidate->progress_methods) {
        strategy.SetTaskRemoved(candidate->task_id, true);
        auto appended_task_id = strategy.AppendTaskForReplay(
            progress_method, candidate->thread_id);
        if (!appended_task_id.has_value()) {
          strategy.SetTaskRemoved(candidate->task_id, false);
          continue;
        }

        log() << "Rollback deadlock attempt: remove "
              << candidate->wait_method << "#" << candidate->task_id
              << ", append " << progress_method << "#"
              << *appended_task_id << " on thread "
              << candidate->thread_id << "\n";

        auto alt = ExploreRound(1);
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

  DualSchedulerWithReplay::Result HandleDeadlock(
      const DualSchedulerWithReplay::FullHistory& full,
      const DualSchedulerWithReplay::SeqHistory& seq) {
    if (deadlock_policy != DeadlockPolicy::Fail) {
      if (!checker.Check(seq)) {
        return DualSchedulerWithReplay::NonLinearizableHistory{
            full, seq,
            DualSchedulerWithReplay::NonLinearizableHistory::Reason::
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
      auto alt = ExploreRound(k);
      if (alt.has_value()) return alt;
      return std::nullopt;
    }

    if (!IsReportableDeadlockHistory(seq)) {
      return std::nullopt;
    }

    return DualSchedulerWithReplay::NonLinearizableHistory{
        full, seq,
        DualSchedulerWithReplay::NonLinearizableHistory::Reason::DEADLOCK};
  }

  // Runs a round with some interleaving while generating it (dual mode)
  DualSchedulerWithReplay::Result RunRound() override {
    DualSchedulerWithReplay::SeqHistory seq;
    DualSchedulerWithReplay::FullHistory full;

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
      if (!t.has_value()) {
        deadlock_detected = true;
        break;
      }
      auto [task, is_new, thread_id_sz] = t.value();
      int thread_id = static_cast<int>(thread_id_sz);

      log() << "step: tid=" << thread_id
            << " task=" << task->GetName()
            << " new=" << std::boolalpha << is_new << "\n";

      // Start event.
      EmitStartEvent(seq, task, is_new, thread_id);
      if (is_new) {
        ++started_tasks;
      }

      full.emplace_back(task);

      task->Resume();

      // Drain dual events emitted from inside the task.
      DrainDual(task, thread_id, seq);

      // Finish.
      if (task->IsReturned()) {
        strategy.OnVerifierTaskFinish(task, thread_id_sz);

        if (!task->IsDual()) {
          auto result = task->GetRetVal();
          seq.emplace_back(Response(task, result, thread_id));
        }
      }
    }

    pretty_printer.PrettyPrint(seq, log());

    if (deadlock_detected) {
      return HandleDeadlock(full, seq);
    }

    if (!checker.Check(seq)) {
      return DualSchedulerWithReplay::NonLinearizableHistory{
          full, seq,
          DualSchedulerWithReplay::NonLinearizableHistory::Reason::
              NON_LINEARIZABLE_HISTORY};
    }

    return std::nullopt;
  }

  // Runs different interleavings of the current round (dual mode)
  DualSchedulerWithReplay::Result ExploreRound(int runs) override {
    for (int i = 0; i < runs; ++i) {
      strategy.ResetExplorationState();

      DualSchedulerWithReplay::SeqHistory seq;
      DualSchedulerWithReplay::FullHistory full;

      bool deadlock_detected{false};

      std::unordered_set<int> started_ids;
      started_ids.reserve(strategy.GetTotalTasksCount());

      for (int tasks_to_run = strategy.GetValidTasksCount();
           tasks_to_run > 0;) {
        auto t = strategy.NextSchedule();
        if (!t.has_value()) {
          deadlock_detected = true;
          break;
        }

        auto [task, is_new, thread_id_sz] = t.value();
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
          // Existing task may no longer be legal in the current replay/exploration
          // state (for example, old unlock after a lock did not complete normally).
          if (!task->IsReturned() &&
              !strategy.VerifyExistingTask(task, thread_id_sz)) {
            deadlock_detected = true;
            break;
          }
        }

        EmitStartEvent(seq, task, first_start_in_explore, thread_id);
        full.emplace_back(task);

        task->Resume();

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
          return DualSchedulerWithReplay::NonLinearizableHistory{
              full, seq,
              DualSchedulerWithReplay::NonLinearizableHistory::Reason::
                  NON_LINEARIZABLE_HISTORY};
        }

        if (!IsReportableDeadlockHistory(seq)) {
          continue;
        }

        return DualSchedulerWithReplay::NonLinearizableHistory{
            full, seq,
            DualSchedulerWithReplay::NonLinearizableHistory::Reason::DEADLOCK};
      }

      if (!checker.Check(seq)) {
        return DualSchedulerWithReplay::NonLinearizableHistory{
            full, seq,
            DualSchedulerWithReplay::NonLinearizableHistory::Reason::
                NON_LINEARIZABLE_HISTORY};
      }
    }

    return std::nullopt;
  }

  // Replays current round with specified interleaving (dual mode).
  //
  // Important:
  // Replay works over a precomputed task ordering. If some existing task is no
  // longer semantically legal in the current replay state, we must not execute
  // it; instead we cut off the rest of that thread in this replay execution.
  //
  // For dual tasks, forced completion on the last occurrence must go through
  // the dual termination path, so the task is marked as FinishedDuringTermination()
  // rather than being mistaken for a normal semantic success.
  DualSchedulerWithReplay::Result ReplayRound(const std::vector<int>& tasks_ordering,
                                              ReplayMode mode) override {
    strategy.ResetCurrentRound();

    std::unordered_set<int> in_ordering;
    in_ordering.reserve(tasks_ordering.size());
    for (int id : tasks_ordering) in_ordering.insert(id);

    DualSchedulerWithReplay::FullHistory full;
    DualSchedulerWithReplay::SeqHistory seq;

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
        std::cerr << "No task with id " << task_id
                  << " exists in round\n";
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

        if (!strategy.VerifyTaskStart(task,
                                      static_cast<size_t>(thread_id_i),
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

      EmitStartEvent(seq, task, is_new, thread_id);
      full.emplace_back(task);

      if (task->IsReturned()) {
        continue;
      }

      const bool is_last = (last_pos[task_id] == step);

      if (mode == ReplayMode::CompleteOnLast && is_last) {
        if (task->IsDual()) {
          bool old_terminating = ltest_round_terminating;
          ltest_round_terminating = true;
          task->Terminate();
          ltest_round_terminating = old_terminating;
        } else {
          task->Terminate();
        }
      } else {
        task->Resume();
      }

      DrainDual(task, thread_id, seq);

      if (task->IsReturned()) {
        strategy.OnVerifierTaskFinish(task, static_cast<size_t>(thread_id_i));

        if (task->FinishedDuringTermination()) {
          thread_cutoff[thread_id] = true;
        }

        if (!task->IsDual()) {
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
          return DualSchedulerWithReplay::NonLinearizableHistory{
              full, seq,
              DualSchedulerWithReplay::NonLinearizableHistory::Reason::
                  NON_LINEARIZABLE_HISTORY};
        }
        if (deadlock_policy != DeadlockPolicy::Fail) {
          return std::nullopt;
        }
        if (!IsReportableDeadlockHistory(seq)) {
          return std::nullopt;
        }
        return DualSchedulerWithReplay::NonLinearizableHistory{
            full, seq,
            DualSchedulerWithReplay::NonLinearizableHistory::Reason::DEADLOCK};
      }
    }

    if (!checker.Check(seq)) {
      return DualSchedulerWithReplay::NonLinearizableHistory{
          full, seq,
          DualSchedulerWithReplay::NonLinearizableHistory::Reason::
              NON_LINEARIZABLE_HISTORY};
    }

    return std::nullopt;
  }

  Strategy& GetStrategy() const override {
    return strategy;
  }

  void Minimize(NonLinearizableHistory& nonlinear_history,
                const RoundMinimizorT<DualHistoryEvent>& minimizor) override {
    minimizor.Minimize(*this, nonlinear_history);
  }

 private:
  Strategy& strategy;
  DualModelChecker& checker;
  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  size_t max_rounds;
  bool should_minimize_history;
  size_t exploration_runs;
  size_t minimization_runs;
  DeadlockPolicy deadlock_policy;
};
