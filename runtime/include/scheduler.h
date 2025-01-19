#pragma once
#include <algorithm>
#include <cassert>
#include <functional>
#include <iterator>
#include <limits>
#include <optional>
#include <random>
#include <string_view>
#include <utility>

#include "lib.h"
#include "lincheck.h"
#include "logger.h"
#include "minimization.h"
#include "minimization_smart.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"
#include "stable_vector.h"

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

// Strategy is the general strategy interface which decides which task
// will be the next one it can be implemented by different strategies, such as:
// randomized/tla/fair
struct Strategy {
  virtual std::optional<size_t> NextThreadId() = 0;

  virtual std::optional<TaskWithMetaData> Next() = 0;

  // Returns the same data as `Next` method. However, it does not generate the
  // round by inserting new tasks in it, but schedules the threads accoding to
  // the strategy policy with previously genereated and saved round (used for
  // round replaying functionality)
  virtual std::optional<TaskWithMetaData> NextSchedule() = 0;

  // Returns { task, its thread id } (TODO: make it `const` method)
  virtual std::optional<std::tuple<Task&, int>> GetTask(int task_id) = 0;

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

  // Removes all tasks to start a new round.
  // (Note: strategy should stop all tasks that already have been started)
  virtual void StartNextRound() = 0;

  // Resets the state of all created tasks in the strategy.
  virtual void ResetCurrentRound() = 0;

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

  virtual ~Strategy() = default;

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
};

template <typename TargetObj, StrategyTaskVerifier Verifier>
struct BaseStrategyWithThreads : public Strategy {
  BaseStrategyWithThreads(size_t threads_count,
                          std::vector<TaskBuilder> constructors)
      : state(std::make_unique<TargetObj>()),
        threads_count(threads_count),
        constructors(std::move(constructors)) {
    round_schedule.resize(threads_count, -1);

    constructors_distribution =
        std::uniform_int_distribution<std::mt19937::result_type>(
            0, constructors.size() - 1);

    // Create queues.
    for (size_t i = 0; i < threads_count; ++i) {
      threads.emplace_back();
    }

    std::random_device dev;
    rng = std::mt19937(dev());
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

  void ResetCurrentRound() override {
    TerminateTasks();
    for (auto& thread : threads) {
      size_t tasks_in_thread = thread.size();
      for (size_t i = 0; i < tasks_in_thread; ++i) {
        if (!IsTaskRemoved(thread[i]->GetId())) {
          thread[i] = thread[i]->Restart(state.get());
        }
      }
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
      // a task has finished or the queue is empty, so we add a new task
      std::shuffle(this->constructors.begin(), this->constructors.end(), rng);
      size_t verified_constructor = -1;
      for (size_t i = 0; i < this->constructors.size(); ++i) {
        TaskBuilder constructor = this->constructors.at(i);
        if (this->sched_checker.Verify(constructor.GetName(), thread_index)) {
          verified_constructor = i;
          break;
        }
      }
      if (verified_constructor == -1) {
        return std::nullopt;
      }
      threads[thread_index].emplace_back(
          this->constructors[verified_constructor].Build(
              this->state.get(), thread_index, this->new_task_id++));
    }

    return TaskWithMetaData{threads[thread_index].back(), is_new, thread_index};
  }

 protected:
  // Terminates all running tasks.
  // We do it in a dangerous way: in random order.
  // Actually, we assume obstruction free here.
  void TerminateTasks() {
    auto& round_schedule = this->round_schedule;
    assert(round_schedule.size() == this->threads.size() &&
           "sizes expected to be the same");
    round_schedule.assign(round_schedule.size(), -1);

    std::vector<size_t> task_indexes(this->threads.size(), 0);
    bool has_nonterminated_threads = true;
    while (has_nonterminated_threads) {
      has_nonterminated_threads = false;

      for (size_t thread_index = 0; thread_index < this->threads.size();
           ++thread_index) {
        auto& thread = this->threads[thread_index];
        auto& task_index = task_indexes[thread_index];

        // find first non-finished task in the thread
        while (task_index < thread.size() && thread[task_index]->IsReturned()) {
          task_index++;
        }

        if (task_index == thread.size()) {
          std::optional<std::string> releaseTask =
              this->sched_checker.ReleaseTask(thread_index);
          // Check if we should schedule release task to unblock other tasks
          if (releaseTask) {
            auto constructor =
                *std::find_if(constructors.begin(), constructors.end(),
                              [=](const TaskBuilder& b) {
                                return b.GetName() == *releaseTask;
                              });
            auto task =
                constructor.Build(this->state.get(), thread_index, task_index);
            auto verified = this->sched_checker.Verify(
                std::string(task->GetName()), thread_index);
            thread.emplace_back(task);
          }
        }

        if (task_index < thread.size() && !thread[task_index]->IsBlocked()) {
          auto& task = thread[task_index];
          has_nonterminated_threads = true;
          // do a single step in this task
          task->Resume();
          if (task->IsReturned()) {
            OnVerifierTaskFinish(task, thread_index);
            debug(stderr, "Terminated: %ld\n", thread_index);
          }
        }
      }
    }
    state.reset(new TargetObj{});
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
  std::uniform_int_distribution<std::mt19937::result_type>
      constructors_distribution;
  std::mt19937 rng;
};

// StrategyScheduler generates different sequential histories (using Strategy)
// and then checks them with the ModelChecker
template <StrategyTaskVerifier Verifier>
struct StrategyScheduler : public SchedulerWithReplay {
  // max_switches represents the maximal count of switches. After this count
  // scheduler will end execution of the Run function
  StrategyScheduler(Strategy& sched_class, ModelChecker& checker,
                    PrettyPrinter& pretty_printer, size_t max_tasks,
                    size_t max_rounds, bool minimize, size_t exploration_runs,
                    size_t minimization_runs)
      : strategy(sched_class),
        checker(checker),
        pretty_printer(pretty_printer),
        max_tasks(max_tasks),
        max_rounds(max_rounds),
        should_minimize_history(minimize),
        exploration_runs(exploration_runs),
        minimization_runs(minimization_runs) {}

  // Run returns full unliniarizable history if such a history is found. Full
  // history is a history with all events, where each element in the vector is a
  // Resume operation on the corresponding task
  Scheduler::Result Run() override {
    for (size_t i = 0; i < max_rounds; ++i) {
      log() << "run round: " << i << "\n";
      auto histories = RunRound();

      if (histories.has_value()) {
        auto& [full_history, sequential_history, reason] = histories.value();

        if (should_minimize_history) {
          log() << "Full nonlinear scenario: \n";
          pretty_printer.PrettyPrint(sequential_history, log());

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

    for (size_t finished_tasks = 0; finished_tasks < max_tasks;) {
      auto t = strategy.Next();
      if (!t.has_value()) {
        deadlock_detected = true;
        break;
      }
      auto [next_task, is_new, thread_id] = t.value();

      // fill the sequential history
      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      full_history.emplace_back(next_task);

      next_task->Resume();
      if (next_task->IsReturned()) {
        finished_tasks++;
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
        debug(stderr, "Tasks finished: %ld\n", finished_tasks);
      }
    }

    pretty_printer.PrettyPrint(sequential_history, log());

    if (deadlock_detected) {
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
      // log() << "Run " << i + 1 << "/" << runs << "\n";
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

        if (is_new) {
          sequential_history.emplace_back(Invoke(next_task, thread_id));
        }
        full_history.emplace_back(next_task);

        next_task->Resume();
        if (next_task->IsReturned()) {
          tasks_to_run--;
          strategy.OnVerifierTaskFinish(next_task, thread_id);

          auto result = next_task->GetRetVal();
          sequential_history.emplace_back(
              Response(next_task, result, thread_id));
        }
      }

      if (deadlock_detected) {
        return NonLinearizableHistory(full_history, sequential_history,
                                      NonLinearizableHistory::Reason::DEADLOCK);
      }

      if (!checker.Check(sequential_history)) {
        // log() << "New nonlinearized scenario:\n";
        // pretty_printer.PrettyPrint(sequential_history, log());
        return NonLinearizableHistory(
            full_history, sequential_history,
            NonLinearizableHistory::Reason::NON_LINEARIZABLE_HISTORY);
      }
    }

    return std::nullopt;
  }

  // Replays current round with specified interleaving
  Result ReplayRound(const std::vector<int>& tasks_ordering) override {
    strategy.ResetCurrentRound();

    // History of invoke and response events which is required for the checker
    FullHistory full_history;
    SeqHistory sequential_history;
    // TODO: `IsRunning` field might be added to `Task` instead
    std::unordered_set<int> started_tasks;
    std::unordered_map<int, int>
        resumes_count;  // task id -> number of appearences in `tasks_ordering`

    for (int next_task_id : tasks_ordering) {
      resumes_count[next_task_id]++;
    }

    for (int next_task_id : tasks_ordering) {
      bool is_new = started_tasks.contains(next_task_id)
                        ? false
                        : started_tasks.insert(next_task_id).second;
      auto task_info = strategy.GetTask(next_task_id);

      if (!task_info.has_value()) {
        std::cerr << "No task with id " << next_task_id << " exists in round"
                  << std::endl;
        throw std::runtime_error("Invalid task id");
      }

      auto [next_task, thread_id] = task_info.value();
      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      full_history.emplace_back(next_task);

      if (next_task->IsReturned()) continue;

      // if this is the last time this task appears in `tasks_ordering`, then
      // complete it fully.
      if (resumes_count[next_task_id] == 0) {
        next_task->Terminate();
      } else {
        resumes_count[next_task_id]--;
        next_task->Resume();
      }

      if (next_task->IsReturned()) {
        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    // pretty_printer.PrettyPrint(sequential_history, log());

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
                const RoundMinimizor& minimizor) override {
    minimizor.Minimize(*this, nonlinear_history);
  }

 private:
  Strategy& strategy;
  ModelChecker& checker;
  PrettyPrinter& pretty_printer;
  size_t max_tasks;
  size_t max_rounds;
  bool should_minimize_history;
  size_t exploration_runs;
  size_t minimization_runs;
};

// TLAScheduler generates all executions satisfying some conditions.
template <typename TargetObj, StrategyTaskVerifier Verifier>
struct TLAScheduler : Scheduler {
  TLAScheduler(size_t max_tasks, size_t max_rounds, size_t threads_count,
               size_t max_switches, size_t max_depth,
               std::vector<TaskBuilder> constructors, ModelChecker& checker,
               PrettyPrinter& pretty_printer, std::function<void()> cancel_func)
      : max_tasks{max_tasks},
        max_rounds{max_rounds},
        max_switches{max_switches},
        constructors{std::move(constructors)},
        checker{checker},
        pretty_printer{pretty_printer},
        max_depth(max_depth),
        cancel(cancel_func) {
    for (size_t i = 0; i < threads_count; ++i) {
      threads.emplace_back(Thread{
          .id = i,
          .tasks = StableVector<Task>{},
      });
    }
    state = std::make_unique<TargetObj>();
  };

  Scheduler::Result Run() override {
    auto [_, res] = RunStep(0, 0);
    return res;
  }

  ~TLAScheduler() { TerminateTasks(); }

 private:
  struct Thread {
    size_t id;
    StableVector<Task> tasks;
  };

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
    // Pointer to the in task thread.
    Task* task{};
    // Is true if the task was created at this step.
    bool is_new{};
  };

  // Terminates all running tasks.
  // We do it in a dangerous way: in random order.
  // Actually, we assume obstruction free here.
  // cancel() func takes care for graceful shutdown
  void TerminateTasks() {
    cancel();
    for (size_t i = 0; i < threads.size(); ++i) {
      for (size_t j = 0; j < threads[i].tasks.size(); ++j) {
        auto& task = threads[i].tasks[j];
        if (!task->IsReturned()) {
          task->Terminate();
        }
      }
    }
  }

  // Replays all actions from 0 to the step_end.
  void Replay(size_t step_end) {
    // Firstly, terminate all running tasks.
    TerminateTasks();
    // In histories we store references, so there's no need to update it.
    state.reset(new TargetObj{});
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
      log() << "run round: " << finished_rounds << "\n";
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
          tasks.emplace_back(cons.Build(&state, i, -1/* TODO: fix task id for tla, because it is Scheduler and not Strategy class for some reason */));
          started_tasks++;
          auto [is_over, res] = ResumeTask(frame, step, switches, thread, true);
          if (is_over || res.has_value()) {
            return {is_over, res};
          }
          tasks.pop_back();
          auto size_after = thread.tasks.size();
          assert(size_before == size_after);
          // As we can't return to the past in coroutine, we need to replay all
          // tasks from the beginning.
          Replay(step);
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
};
