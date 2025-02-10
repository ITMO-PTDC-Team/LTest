#include "include/scheduler.h"

#include "include/logger.h"
#include "include/pretty_print.h"

StrategyScheduler::StrategyScheduler(Strategy &sched_class,
                                     ModelChecker &checker,
                                     PrettyPrinter &pretty_printer,
                                     size_t max_tasks, size_t max_rounds,
                                     verify_state_func valid,
                                     bool no_extra_tasks)
    : strategy(sched_class),
      checker(checker),
      pretty_printer(pretty_printer),
      max_tasks(max_tasks),
      max_rounds(max_rounds),
      no_extra_tasks(no_extra_tasks) {}

Scheduler::Result StrategyScheduler::runRound() {
  // History of invoke and response events which is required for the checker
  std::vector<std::variant<Invoke, Response>> sequential_history;
  // Full history of the current execution in the Run function
  std::vector<std::reference_wrapper<Task>> full_history;

  for (size_t finished_tasks = 0; finished_tasks < max_tasks;) {
    auto [next_task, is_new, thread_id] = strategy.Next();
    // TODO: how to revert a invalid task?
    //  fill the sequential history
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

Scheduler::Result StrategyScheduler::Run() {
  for (size_t i = 0; i < max_rounds; ++i) {
    log() << "run round: " << i << "\n";
    auto seq_history = runRound();
    if (seq_history.has_value()) {
      return seq_history;
    }
    log() << "===============================================\n\n";
    log().flush();
    strategy.StartNextRound();
  }

  return std::nullopt;
}
