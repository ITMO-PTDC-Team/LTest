#include "minimization.h"

#include "scheduler.h"

// round minimizor interface
std::vector<int> RoundMinimizor::GetTasksOrdering(
    const Scheduler::FullHistory& full_history,
    const std::unordered_set<int> exclude_task_ids) {
  std::vector<int> tasks_ordering;

  for (auto& task : full_history) {
    if (exclude_task_ids.contains(task.get()->GetId())) continue;
    tasks_ordering.emplace_back(task.get()->GetId());
  }

  return tasks_ordering;
}

// greedy
void GreedyRoundMinimizor::Minimize(
    SchedulerWithReplay& sched,
    Scheduler::NonLinearizableHistory& nonlinear_history) const {
  std::vector<std::reference_wrapper<const Task>> tasks;

  for (const HistoryEvent& event : nonlinear_history.seq) {
    if (std::holds_alternative<Invoke>(event)) {
      tasks.push_back(std::get<Invoke>(event).GetTask());
    }
  }

  Strategy& strategy = sched.GetStrategy();
  // remove single task
  for (auto& task : tasks) {
    if (strategy.IsTaskRemoved(task.get()->GetId())) continue;

    // log() << "Try to remove task with id: " << task.get()->GetId() << "\n";
    auto new_histories =
        OnTasksRemoved(sched, nonlinear_history, {task.get()->GetId()});

    if (new_histories.has_value()) {
      nonlinear_history.full.swap(new_histories.value().full);
      nonlinear_history.seq.swap(new_histories.value().seq);
      strategy.SetTaskRemoved(task.get()->GetId(), true);
    }
  }

  // remove two tasks (for operations with semantics of add/remove)
  for (size_t i = 0; i < tasks.size(); ++i) {
    auto& task_i = tasks[i].get();
    int task_i_id = task_i->GetId();
    if (strategy.IsTaskRemoved(task_i_id)) continue;

    for (size_t j = i + 1; j < tasks.size(); ++j) {
      auto& task_j = tasks[j].get();
      int task_j_id = task_j->GetId();
      if (strategy.IsTaskRemoved(task_j_id)) continue;

      // log() << "Try to remove tasks with ids: " << task_i.get()->GetId() << "
      // and "
      //       << task_j.get()->GetId() << "\n";
      auto new_histories =
          OnTasksRemoved(sched, nonlinear_history, {task_i_id, task_j_id});

      if (new_histories.has_value()) {
        // sequential history (Invoke/Response events) could have odd number of
        // history events in case if some task are not completed which is allowed by linearizability checker
        nonlinear_history.full.swap(new_histories.value().full);
        nonlinear_history.seq.swap(new_histories.value().seq);

        strategy.SetTaskRemoved(task_i_id, true);
        strategy.SetTaskRemoved(task_j_id, true);
        break;  // tasks (i, j) were removed, so go to the next iteration of i
      }
    }
  }

  // replay minimized round one last time to put coroutines in `returned` state
  // (because multiple failed attempts to minimize new scenarios could leave
  // tasks in invalid state)
  sched.ReplayRound(
      RoundMinimizor::GetTasksOrdering(nonlinear_history.full, {}));
}

// same interleaving
Scheduler::Result SameInterleavingMinimizor::OnTasksRemoved(
    SchedulerWithReplay& sched,
    const Scheduler::NonLinearizableHistory& nonlinear_history,
    const std::unordered_set<int>& task_ids) const {
  std::vector<int> new_ordering =
      RoundMinimizor::GetTasksOrdering(nonlinear_history.full, task_ids);
  return sched.ReplayRound(new_ordering);
}

// strategy exploration
StrategyExplorationMinimizor::StrategyExplorationMinimizor(int runs_)
    : runs(runs_) {}

Scheduler::Result StrategyExplorationMinimizor::OnTasksRemoved(
    SchedulerWithReplay& sched,
    const Scheduler::NonLinearizableHistory& nonlinear_history,
    const std::unordered_set<int>& task_ids) const {
  auto mark_tasks_as_removed = [&](bool is_removed) {
    for (const auto& task_id : task_ids) {
      sched.GetStrategy().SetTaskRemoved(task_id, is_removed);
    }
  };

  mark_tasks_as_removed(true);
  Scheduler::Result new_histories = sched.ExploreRound(runs);
  if (!new_histories.has_value()) {
    mark_tasks_as_removed(false);
  }

  return new_histories;
}