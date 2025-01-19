#pragma once
#include <unordered_set>

#include "scheduler_fwd.h"

/**
 * This is an interface for minimizors that decrease the number of tasks in the
 * nonlinearized history.
 */
struct RoundMinimizor {
  // Minimizes the number of tasks in the nonlinearized history; modifies
  // argument `nonlinear_history`.
  virtual void Minimize(
      SchedulerWithReplay& sched,
      Scheduler::NonLinearizableHistory& nonlinear_history) const = 0;

  /**
   * Returns ids of tasks taken from `full_history`, excluding those ids, that
   * are specified in `exclude_task_ids`.
   */
  static std::vector<int> GetTasksOrdering(
      const Scheduler::FullHistory& full_history,
      std::unordered_set<int> exclude_task_ids);
};

/**
 * This is a base class for minimizors that try to remove tasks from the
 * nonlinearized history greedily, using the callback `OnTasksRemoved` with
 * passed tasks.
 */
struct GreedyRoundMinimizor : public RoundMinimizor {
  /**
   * Greedy round minimizor iterates over all non-removed tasks in the
   * nonlinearized history and tries to remove them one by one.
   *
   * Also it has an additional cycle in which the minimizor tries to select all
   * pairs of tasks and remove them together, this is done to account for
   * data-structures that have the `add/remove` semantics.
   */
  void Minimize(
      SchedulerWithReplay& sched,
      Scheduler::NonLinearizableHistory& nonlinear_history) const override;

 protected:
  /**
   * This method is called when the minimizor tries to remove tasks from the
   * nonlinearized history. Greedy minimizors call this method for all tasks
   * with ids `i` and for all pairs of tasks `(i, j)`, where `i < j`.
   * @param sched the scheduler that will replay the round with some tasks
   * marked as removed.
   * @param nonlinear_history the nonlinearized history that is being minimized.
   * @param task_ids the task ids that the minimizor tries to remove.
   * @return the new nonlinearized history if the tasks were successfully
   * removed, `std::nullopt` otherwise.
   */
  virtual Scheduler::Result OnTasksRemoved(
      SchedulerWithReplay& sched,
      const Scheduler::NonLinearizableHistory& nonlinear_history,
      const std::unordered_set<int>& task_ids) const = 0;
};

/**
 * `SameInterleavingMinimizor` minimizor uses passed to it `nonlinear_history`
 * and just removes provided tasks from the full and sequential histories
 * without changing the rest of the interleaving.
 *
 * So if the initial full history looked like this `[1, 1, 2, 3, 2, 2, 1, 3]`
 * (task ids) and we want to remove task with id 2, the new full history will be
 * `[1, 1, 3, 1, 3]`.
 */
struct SameInterleavingMinimizor : public GreedyRoundMinimizor {
 protected:
  virtual Scheduler::Result OnTasksRemoved(
      SchedulerWithReplay& sched,
      const Scheduler::NonLinearizableHistory& nonlinear_history,
      const std::unordered_set<int>& task_ids) const override;
};

/**
 * `StrategyExplorationMinimizor` minimizor tries to remove tasks from the
 * nonlinearized history and explores different interleavings of the current
 * round.
 *
 * It uses the `runs` parameter to determine how many times it should try to
 * find the new nonlinearized history with provided tasks marked as removed. If
 * such new history is not found in a given number of `runs`, the the tasks are
 * put in their initial non-removed state.
 */
struct StrategyExplorationMinimizor : public GreedyRoundMinimizor {
  StrategyExplorationMinimizor() = delete;
  explicit StrategyExplorationMinimizor(int runs_);

 protected:
  virtual Scheduler::Result OnTasksRemoved(
      SchedulerWithReplay& sched,
      const Scheduler::NonLinearizableHistory& nonlinear_history,
      const std::unordered_set<int>& task_ids) const override;

 private:
  int runs;
};