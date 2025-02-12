#include <numeric>

#include "include/scheduler.h"
#include "include/logger.h"
#include "include/pretty_print.h"
#include "include/minimization.h"
#include "include/minimization_smart.h"

StrategyScheduler::StrategyScheduler(Strategy &sched_class,
                                     ModelChecker &checker,
                                     PrettyPrinter &pretty_printer,
                                     size_t max_tasks,
                                     size_t max_rounds,
                                     size_t exploration_runs,
                                     size_t minimization_runs)
    : strategy(sched_class),
      checker(checker),
      pretty_printer(pretty_printer),
      max_tasks(max_tasks),
      max_rounds(max_rounds),
      exploration_runs(exploration_runs),
      minimization_runs(minimization_runs) {}

Scheduler::Result StrategyScheduler::RunRound() {
  // History of invoke and response events which is required for the checker
  SeqHistory sequential_history;
  // Full history of the current execution in the Run function
  FullHistory full_history;

  for (size_t finished_tasks = 0; finished_tasks < max_tasks;) {
    auto [next_task, is_new, thread_id] = strategy.Next();

    // fill the sequential history
    if (is_new) {
      sequential_history.emplace_back(Invoke(next_task, thread_id));
    }
    full_history.emplace_back(next_task);

    next_task->Resume();
    if (next_task->IsReturned()) {
      finished_tasks++;

      auto result = next_task->GetRetVal();
      sequential_history.emplace_back(Response(next_task, result, thread_id));
    }
  }

  pretty_printer.PrettyPrint(sequential_history, log());

  if (!checker.Check(sequential_history)) {
    return std::make_pair(full_history, sequential_history);
  }

  return std::nullopt;
}

StrategyScheduler::Result StrategyScheduler::ExploreRound(int runs) {
  for (int i = 0; i < runs; ++i) {
    // log() << "Run " << i + 1 << "/" << runs << "\n";
    strategy.ResetCurrentRound();
    SeqHistory sequential_history;
    FullHistory full_history;

    for (int tasks_to_run = strategy.GetValidTasksCount(); tasks_to_run > 0;) {
      auto [next_task, is_new, thread_id] = strategy.NextSchedule();

      if (is_new) {
        sequential_history.emplace_back(Invoke(next_task, thread_id));
      }
      full_history.emplace_back(next_task);

      next_task->Resume();
      if (next_task->IsReturned()) {
        tasks_to_run--;

        auto result = next_task->GetRetVal();
        sequential_history.emplace_back(Response(next_task, result, thread_id));
      }
    }

    if (!checker.Check(sequential_history)) {
      // log() << "New nonlinearized scenario:\n";
      // pretty_printer.PrettyPrint(sequential_history, log());
      return std::make_pair(full_history, sequential_history);
    }
  }

  return std::nullopt;
}

StrategyScheduler::Result StrategyScheduler::ReplayRound(const std::vector<int>& tasks_ordering) {
  strategy.ResetCurrentRound();

  // History of invoke and response events which is required for the checker
  FullHistory full_history;
  SeqHistory sequential_history;
  // TODO: `IsRunning` field might be added to `Task` instead
  std::unordered_set<int> started_tasks;
  std::unordered_map<int, int> resumes_count; // task id -> number of appearences in `tasks_ordering`

  for (int next_task_id : tasks_ordering) {
    resumes_count[next_task_id]++;
  }

  for (int next_task_id : tasks_ordering) {
    bool is_new = started_tasks.contains(next_task_id) ? false : started_tasks.insert(next_task_id).second;
    auto task_info = strategy.GetTask(next_task_id);

    if (!task_info.has_value()) {
      std::cerr << "No task with id " << next_task_id << " exists in round" << std::endl;
      throw std::runtime_error("Invalid task id");
    }

    auto [next_task, thread_id] = task_info.value();
    if (is_new) {
      sequential_history.emplace_back(Invoke(next_task, thread_id));
    }
    full_history.emplace_back(next_task);

    if (next_task->IsReturned()) continue;

    // if this is the last time this task appears in `tasks_ordering`, then complete it fully.
    if (resumes_count[next_task_id] == 0) {
      next_task->Terminate();
    }
    else {
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
    return std::make_pair(full_history, sequential_history);
  }

  return std::nullopt;
}

Strategy& StrategyScheduler::GetStrategy() const {
  return strategy;
}

void StrategyScheduler::Minimize(
  Scheduler::BothHistories& nonlinear_history,
  const RoundMinimizor& minimizor
) {
  minimizor.Minimize(*this, nonlinear_history);
}

Scheduler::Result StrategyScheduler::Run() {
  for (size_t i = 0; i < max_rounds; ++i) {
    log() << "run round: " << i << "\n";
    auto histories = RunRound();

    if (histories.has_value()) {
      auto& [full_history, sequential_history] = histories.value();

      log() << "Full nonlinear scenario: \n";
      pretty_printer.PrettyPrint(sequential_history, log());
      
      log() << "Minimizing same interleaving...\n";
      Minimize(histories.value(), SameInterleavingMinimizor());
      log() << "Minimized to:\n";
      pretty_printer.PrettyPrint(sequential_history, log());

      log() << "Minimizing with rescheduling (exploration runs: " << exploration_runs << ")...\n";
      Minimize(histories.value(), StrategyExplorationMinimizor(exploration_runs));
      log() << "Minimized to:\n";
      pretty_printer.PrettyPrint(sequential_history, log());

      log() << "Minimizing with smart minimizor (exploration runs: "
            << exploration_runs << ", minimization runs: "
            << minimization_runs << ")...\n";
      Minimize(histories.value(), SmartMinimizor(exploration_runs, minimization_runs, pretty_printer));

      return histories;
    }
    log() << "===============================================\n\n";
    log().flush();
    strategy.StartNextRound();
  }

  return std::nullopt;
}