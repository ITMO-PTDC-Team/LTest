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

#include "custom_round.h"
#include "lib.h"
#include "lincheck.h"
#include "logger.h"
#include "minimization.h"
#include "minimization_smart.h"
#include "os_simulator.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"
#include "stable_vector.h"

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

// Strategy is the general strategy interface which decides which task
// will be the next one it can be implemented by different strategies, such as:
// randomized/tla/fair
struct Strategy {
  virtual bool IsExhausted() = 0;
  virtual std::optional<size_t> NextThreadId() = 0;

  virtual StrategyNextResult Next() = 0;

  // Returns the same data as `Next` method. However, it does not generate the
  // round by inserting new tasks in it, but schedules the threads accoding to
  // the strategy policy with previously genereated and saved round (used for
  // round replaying functionality)
  virtual StrategyNextResult NextSchedule() = 0;

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

  void StartNextRound() override {
    this->new_task_id = 0;
    // also resets the state
    this->TerminateTasks();
    simulator.ResetState();

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
  }

  void ResetCurrentRound() override {
    TerminateTasks();
    for (auto& thread : threads) {
      size_t tasks_in_thread = thread.size();
      for (size_t i = 0; i < tasks_in_thread; ++i) {
        if (!IsTaskRemoved(thread[i]->GetId())) {
          thread[i]->Restart(state.get());
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
    this->state = std::make_unique<TargetObj>();

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

  StrategyNextResult Next() override { return NextVerifiedFor(NextThreadId()); }

  StrategyNextResult NextVerifiedFor(std::optional<size_t> opt_thread_index) {
    if (!opt_thread_index.has_value()) {
      return DEADLOCK;
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
        if (this->sched_checker.Verify(constructor.GetName(), thread_index,
                                       is_new)) {
          verified_constructor = i;
          break;
        }
      }
      if (verified_constructor == -1) {
        return DEADLOCK;
      }
      threads[thread_index].emplace_back(
          this->constructors[verified_constructor].Build(
              this->state.get(), thread_index, this->new_task_id++));
    }

    return TaskWithMetaData{threads[thread_index].back(), is_new, thread_index};
  }

 protected:
  void TerminateTasks() {
    auto& round_schedule = this->round_schedule;
    assert(round_schedule.size() == this->threads.size() &&
           "sizes expected to be the same");
    round_schedule.assign(round_schedule.size(), -1);
    sched_checker.Reset();
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
  OSSimulator simulator;
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
                    std::vector<CustomRound> custom_rounds,
                    PrettyPrinter& pretty_printer, size_t max_tasks,
                    size_t max_rounds, bool minimize, size_t exploration_runs,
                    size_t minimization_runs)
      : strategy(sched_class),
        checker(checker),
        custom_rounds(std::move(custom_rounds)),
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
    for (size_t j = 0; j < custom_rounds.size() + max_rounds; ++j) {
      bool is_running_custom_scenarios = (j < custom_rounds.size());
      Result histories;

      if (is_running_custom_scenarios) {
        log() << "explore custom round: " << j << "\n";
        debug(stderr, "explore custom round: %zu\n", j);
        strategy.SetCustomRound(custom_rounds[j]);
        histories = ExploreRound(exploration_runs, true);
      } else {
        size_t i = j - custom_rounds.size();
        log() << "run round: " << i << "\n";
        debug(stderr, "run round: %zu\n", i);
        histories = RunRound();
      }
      if (strategy.IsExhausted()) {
        break;
      }

      if (histories.has_value()) {
        auto& [full_history, sequential_history, reason] = histories.value();
        int threads_num = GetStrategyThreadsCount();

        if (should_minimize_history) {
          log() << "Full nonlinear scenario: \n";
          pretty_printer.PrettyPrint(sequential_history, threads_num, log());

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

        return histories;
      }
      log() << "===============================================\n\n";
      log().flush();
      strategy.StartNextRound();
    }

    return std::nullopt;
  }

  int GetStrategyThreadsCount() const override {
    return strategy.GetThreadsCount();
  }

 protected:
  // Runs a round with some interleaving while generating it
  Result RunRound() override {
    // History of invoke and response events which is required for the checker
    SeqHistory sequential_history;
    // Full history of the current execution in the Run function
    FullHistoryWithThreads full_history;

    bool deadlock_detected{false};

    for (size_t finished_tasks = 0; finished_tasks < max_tasks;) {
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

      // fill the sequential history
      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      next_task->Resume();
      UpdateFullHistory(full_history, thread_id, next_task, is_new);
      if (next_task->IsReturned()) {
        finished_tasks++;
        strategy.OnVerifierTaskFinish(next_task, thread_id);

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
        debug(stderr, "Tasks finished: %ld\n", finished_tasks);
      }
    }

    pretty_printer.PrettyPrint(sequential_history, GetStrategyThreadsCount(),
                               log());

    if (deadlock_detected) {
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

  // Runs different interleavings of the current round
  Result ExploreRound(int runs, bool log_each_interleaving) override {
    for (int i = 0; i < runs; ++i) {
      // log() << "Run " << i + 1 << "/" << runs << "\n";
      strategy.ResetCurrentRound();
      SeqHistory sequential_history;
      FullHistory full_history;

      bool deadlock_detected{false};

      for (int tasks_to_run = strategy.GetValidTasksCount();
           tasks_to_run > 0;) {
        auto t = strategy.NextSchedule();
        if (auto f = std::get_if<StrategyNextScheduleFailure>(&t)) {
          if (*f == DEADLOCK) {
            deadlock_detected = true;
            break;
          }
          if (*f == NEED_REPLAY) {
            strategy.StartNextRound();
            sequential_history.clear();
            full_history.clear();
            tasks_to_run = 0;
            continue;
          }
        }
        auto [next_task, is_new, thread_id] = std::get<TaskWithMetaData>(t);

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

      if (log_each_interleaving) {
        pretty_printer.PrettyPrint(sequential_history,
                                   GetStrategyThreadsCount(), log());
        log() << "\n";
      }

      if (deadlock_detected) {
        return NonLinearizableHistory(full_history, sequential_history,
                                      NonLinearizableHistory::Reason::DEADLOCK);
      }

      if (!checker.Check(sequential_history)) {
        // log() << "New nonlinearized scenario:\n";
        // pretty_printer.PrettyPrint(sequential_history,
        //                            GetStartegyThreadsCount(), log());
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
        while (!next_task->IsReturned()) {
          next_task->Resume();
        }
      } else {
        resumes_count[next_task_id]--;
        next_task->Resume();
      }

      if (next_task->IsReturned()) {
        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    // pretty_printer.PrettyPrint(sequential_history, GetStrategyThreadsCount(),
    // log());

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

  void UpdateFullHistory(FullHistoryWithThreads& full_history, size_t thread_id,
                         Task& task, bool is_new) {
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
};
