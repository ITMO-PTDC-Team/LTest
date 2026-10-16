#pragma once
#include <algorithm>
#include <optional>
#include <stdexcept>
#include <unordered_set>
#include <vector>

#include "attempt.h"

namespace ltest {

/**
 * This is an interface for minimizors that decrease the number of tasks in a
 * non-linearizable history.
 */
struct RoundMinimizor {
  using ResultT = std::optional<AttemptReport>;

  virtual ~RoundMinimizor() = default;

  // Minimizes the number of tasks in the non-linearizable history; modifies
  // argument `nonlinear_history`.
  virtual void Minimize(CandidateRunner& sched,
                        AttemptReport& nonlinear_history) const = 0;

 protected:
  static ResultT ReplayRound(CandidateRunner& sched,
                             const AttemptRequest& request) {
    auto report = sched(request);
    if (report && report->result == InfrastructureFailure)
      throw std::runtime_error("forked replay failed");
    if (report && MeaningfulFailure(*report)) return report;
    return std::nullopt;
  }

  static ResultT ExploreRound(CandidateRunner& sched,
                              const AttemptRequest& request, int runs) {
    for (int run = 0; run < runs; ++run) {
      auto report = sched(request);
      if (!report) break;
      if (report->result == InfrastructureFailure)
        throw std::runtime_error("forked exploration failed");
      if (MeaningfulFailure(*report)) return report;
    }
    return std::nullopt;
  }
};

/**
 * This is a base class for minimizors that try to remove tasks from the
 * non-linearizable history greedily, using the callback `OnTasksRemoved` with
 * passed tasks.
 */
struct GreedyRoundMinimizor : public RoundMinimizor {
  /**
   * Greedy round minimizor iterates over all non-removed tasks in the
   * non-linearizable history and tries to remove them one by one.
   *
   * It also has an additional cycle where the minimizor tries to remove all
   * pairs of tasks together. This accounts for data structures with add/remove
   * style semantics where the bug may need both sides to stay or disappear
   * together.
   */
  void Minimize(CandidateRunner& sched,
                AttemptReport& nonlinear_history) const override {
    // Collect operation starts from the report.
    std::vector<int> op_ids;
    op_ids.reserve(nonlinear_history.events.size());

    std::unordered_set<int> seen;
    seen.reserve(nonlinear_history.events.size());

    for (const auto& ev : nonlinear_history.events) {
      if (!ev.IsStart()) continue;
      if (seen.insert(ev.operation).second) {
        op_ids.push_back(ev.operation);
      }
    }

    auto is_removed = [&](int id) {
      return std::none_of(
          nonlinear_history.operations.begin(),
          nonlinear_history.operations.end(),
          [&](const auto& operation) { return operation.id == id; });
    };

    // remove single task
    for (int id : op_ids) {
      if (is_removed(id)) continue;

      auto new_histories = OnTasksRemoved(sched, nonlinear_history, {id});
      if (new_histories.has_value()) {
        nonlinear_history = std::move(*new_histories);
      }
    }

    // remove two tasks (for add/remove-like semantics)
    for (size_t i = 0; i < op_ids.size(); ++i) {
      int id_i = op_ids[i];
      if (is_removed(id_i)) continue;

      for (size_t j = i + 1; j < op_ids.size(); ++j) {
        int id_j = op_ids[j];
        if (is_removed(id_j)) continue;

        auto new_histories =
            OnTasksRemoved(sched, nonlinear_history, {id_i, id_j});
        if (new_histories.has_value()) {
          nonlinear_history = std::move(*new_histories);
          break;
        }
      }
    }
  }

 protected:
  /**
   * This method is called when the minimizor tries to remove tasks from the
   * non-linearizable history. Greedy minimizors call it for all task ids `i`
   * and for all pairs of task ids `(i, j)`, where `i < j`.
   *
   * @param sched the callback that executes the reduced attempt.
   * @param nonlinear_history the non-linearizable history being minimized.
   * @param task_ids the task ids that the minimizor tries to remove.
   * @return the new non-linearizable history if the tasks were successfully
   * removed, `std::nullopt` otherwise.
   */
  virtual ResultT OnTasksRemoved(
      CandidateRunner& sched, const AttemptReport& nonlinear_history,
      const std::unordered_set<int>& task_ids) const = 0;
};

/**
 * `SameInterleavingMinimizor` uses the provided non-linearizable history and
 * removes selected operations from the attempt without changing the rest
 * of the scheduling order.
 *
 * If the initial ordering looked like this `[1, 1, 2, 3, 2, 2, 1, 3]`
 * (task ids), and we remove task with id 2, the new ordering will be
 * `[1, 1, 3, 1, 3]`.
 */
struct SameInterleavingMinimizor : public GreedyRoundMinimizor {
 protected:
  ResultT OnTasksRemoved(
      CandidateRunner& sched, const AttemptReport& nonlinear_history,
      const std::unordered_set<int>& task_ids) const override {
    auto request = AttemptRequest(nonlinear_history, AttemptKind::FixedReplay);
    RemoveOperations(request, task_ids);
    // Replay every recorded step and retain genuine completion events.
    return ReplayRound(sched, request);
  }
};

/**
 * `StrategyExplorationMinimizor` tries to remove tasks from the
 * non-linearizable history and then explores different interleavings of the
 * current round.
 *
 * It uses the `runs` parameter to determine how many times it should try to
 * find a new non-linearizable history with the provided tasks marked as
 * removed. If such history is not found in a given number of runs, the tasks
 * are returned to their initial non-removed state.
 */
struct StrategyExplorationMinimizor : public GreedyRoundMinimizor {
  StrategyExplorationMinimizor() = delete;
  explicit StrategyExplorationMinimizor(int runs_) : runs(runs_) {}

 protected:
  ResultT OnTasksRemoved(
      CandidateRunner& sched, const AttemptReport& nonlinear_history,
      const std::unordered_set<int>& task_ids) const override {
    auto request = AttemptRequest(nonlinear_history, AttemptKind::Reschedule);
    RemoveOperations(request, task_ids);
    return ExploreRound(sched, request, runs);
  }

 private:
  int runs;
};

}  // namespace ltest
