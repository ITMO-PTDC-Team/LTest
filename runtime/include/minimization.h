#pragma once
#include <optional>
#include <unordered_set>
#include <unordered_map>
#include <vector>

#include "scheduler_fwd.h"

namespace ltest {

// Traits used by minimizers to detect which history events start visible
// operations. Ordinary histories start operations with Invoke. Dual histories
// start blocking operations with RequestInvoke and ordinary operations with
// Invoke.
template <class Event>
struct MinTraits;

template <>
struct MinTraits<HistoryEvent> {
  static std::optional<int> StartTaskId(const HistoryEvent& e) {
    if (auto inv = std::get_if<Invoke>(&e)) {
      return inv->GetTask()->GetId();
    }
    return std::nullopt;
  }
};

template <>
struct MinTraits<DualHistoryEvent> {
  static std::optional<int> StartTaskId(const DualHistoryEvent& e) {
    if (auto req = std::get_if<RequestInvoke>(&e)) {
      return req->GetTask()->GetId();
    }
    if (auto inv = std::get_if<Invoke>(&e)) {
      return inv->GetTask()->GetId();
    }
    return std::nullopt;
  }
};

// Traits used to decide whether a minimized bad history still contains a
// visible pending operation. This rejects degenerate deadlock scenarios where
// minimization removed all meaningful work and left only a scheduler artifact.
template <class Event>
struct MinBadTraits;

template <>
struct MinBadTraits<HistoryEvent> {
  static bool HasPendingVisibleOp(const std::vector<HistoryEvent>& seq) {
    std::unordered_set<int> started;
    std::unordered_set<int> finished;

    for (const auto& ev : seq) {
      if (auto inv = std::get_if<Invoke>(&ev)) {
        started.insert(inv->GetTask()->GetId());
      } else if (auto resp = std::get_if<Response>(&ev)) {
        finished.insert(resp->GetTask()->GetId());
      }
    }

    for (int id : started) {
      if (!finished.contains(id)) {
        return true;
      }
    }
    return false;
  }
};

template <>
struct MinBadTraits<DualHistoryEvent> {
  static bool HasPendingVisibleOp(const std::vector<DualHistoryEvent>& seq) {
    std::unordered_set<int> ordinary_started;
    std::unordered_set<int> ordinary_finished;

    std::unordered_set<int> dual_started;
    std::unordered_set<int> dual_finished;

    for (const auto& ev : seq) {
      if (auto inv = std::get_if<Invoke>(&ev)) {
        ordinary_started.insert(inv->GetTask()->GetId());
      } else if (auto resp = std::get_if<Response>(&ev)) {
        ordinary_finished.insert(resp->GetTask()->GetId());
      } else if (auto req = std::get_if<RequestInvoke>(&ev)) {
        dual_started.insert(req->GetTask()->GetId());
      } else if (auto fol_resp = std::get_if<FollowUpResponse>(&ev)) {
        dual_finished.insert(fol_resp->GetTask()->GetId());
      }
    }

    for (int id : ordinary_started) {
      if (!ordinary_finished.contains(id)) {
        return true;
      }
    }

    for (int id : dual_started) {
      if (!dual_finished.contains(id)) {
        return true;
      }
    }

    return false;
  }
};

/**
 * This is an interface for minimizors that decrease the number of tasks in a
 * non-linearizable history.
 *
 * Event is HistoryEvent for ordinary histories and DualHistoryEvent for dual
 * histories.
 */
template <class Event>
struct RoundMinimizorT {
  using SchedulerT = BasicSchedulerWithReplay<Event>;
  using NonLinearizableHistoryT =
      typename BasicScheduler<Event>::NonLinearizableHistory;
  using FullHistoryT = typename BasicScheduler<Event>::FullHistory;
  using ResultT = typename BasicScheduler<Event>::Result;

  virtual ~RoundMinimizorT() = default;

  // Minimizes the number of tasks in the non-linearizable history; modifies
  // argument `nonlinear_history`.
  virtual void Minimize(SchedulerT& sched,
                        NonLinearizableHistoryT& nonlinear_history) const = 0;

  /**
   * Returns ids of tasks taken from `full_history`, excluding ids specified in
   * `exclude_task_ids`.
   */
  static std::vector<int> GetTasksOrdering(
      const FullHistoryT& full_history,
      const std::unordered_set<int>& exclude_task_ids) {
    std::vector<int> tasks_ordering;
    tasks_ordering.reserve(full_history.size());

    for (auto& task_ref : full_history) {
      int id = task_ref.get()->GetId();
      if (exclude_task_ids.contains(id)) continue;
      tasks_ordering.emplace_back(id);
    }
    return tasks_ordering;
  }

  static bool HasAnyStartedOp(const std::vector<Event>& seq) {
    for (const auto& ev : seq) {
      if (MinTraits<Event>::StartTaskId(ev).has_value()) {
        return true;
      }
    }
    return false;
  }

  static bool IsMeaningfulBadResult(
      const NonLinearizableHistoryT& bad_history) {
    // Reject completely empty / no-op bad histories.
    if (!HasAnyStartedOp(bad_history.seq)) {
      return false;
    }

    // For deadlock require at least one visibly pending operation.
    // Otherwise minimization may collapse to a degenerate "deadlock"
    // that is just a replay/scheduler artifact.
    if (bad_history.reason == NonLinearizableHistoryT::Reason::DEADLOCK &&
        !MinBadTraits<Event>::HasPendingVisibleOp(bad_history.seq)) {
      return false;
    }

    return true;
  }
};

/**
 * This is a base class for minimizors that try to remove tasks from the
 * non-linearizable history greedily, using the callback `OnTasksRemoved` with
 * passed tasks.
 */
template <class Event>
struct GreedyRoundMinimizorT : public RoundMinimizorT<Event> {
  using Base = RoundMinimizorT<Event>;
  using SchedulerT = typename Base::SchedulerT;
  using NonLinearizableHistoryT = typename Base::NonLinearizableHistoryT;
  using ResultT = typename Base::ResultT;

  /**
   * Greedy round minimizor iterates over all non-removed tasks in the
   * non-linearizable history and tries to remove them one by one.
   *
   * It also has an additional cycle where the minimizor tries to remove all
   * pairs of tasks together. This accounts for data structures with add/remove
   * style semantics where the bug may need both sides to stay or disappear
   * together.
   */
  void Minimize(SchedulerT& sched,
                NonLinearizableHistoryT& nonlinear_history) const override {
    // Collect operation ids (starts) from seq history.
    std::vector<int> op_ids;
    op_ids.reserve(nonlinear_history.seq.size());

    std::unordered_set<int> seen;
    seen.reserve(nonlinear_history.seq.size());

    for (const auto& ev : nonlinear_history.seq) {
      auto id = MinTraits<Event>::StartTaskId(ev);
      if (!id.has_value()) continue;
      if (seen.insert(*id).second) {
        op_ids.push_back(*id);
      }
    }

    Strategy& strategy = sched.GetStrategy();

    // remove single task
    for (int id : op_ids) {
      if (strategy.IsTaskRemoved(id)) continue;

      auto new_histories = OnTasksRemoved(sched, nonlinear_history, {id});
      if (new_histories.has_value() &&
          Base::IsMeaningfulBadResult(new_histories.value())) {
        nonlinear_history.full.swap(new_histories->full);
        nonlinear_history.seq.swap(new_histories->seq);
        nonlinear_history.reason = new_histories->reason;
        strategy.SetTaskRemoved(id, true);
      }
    }

    // remove two tasks (for add/remove-like semantics)
    for (size_t i = 0; i < op_ids.size(); ++i) {
      int id_i = op_ids[i];
      if (strategy.IsTaskRemoved(id_i)) continue;

      for (size_t j = i + 1; j < op_ids.size(); ++j) {
        int id_j = op_ids[j];
        if (strategy.IsTaskRemoved(id_j)) continue;

        auto new_histories =
            OnTasksRemoved(sched, nonlinear_history, {id_i, id_j});
        if (new_histories.has_value() &&
            Base::IsMeaningfulBadResult(new_histories.value())) {
          nonlinear_history.full.swap(new_histories->full);
          nonlinear_history.seq.swap(new_histories->seq);
          nonlinear_history.reason = new_histories->reason;
          strategy.SetTaskRemoved(id_i, true);
          strategy.SetTaskRemoved(id_j, true);
          break;
        }
      }
    }

    // Put coroutines in returned state (cleanup after many failed attempts).
    // Final cleanup: put tasks into a valid returned state.
    // For DEADLOCK, CompleteOnLast replay can spin forever on permanently
    // waiting ops, so we rely on ResetCurrentRound().
    if (nonlinear_history.reason ==
        BasicScheduler<Event>::NonLinearizableHistory::Reason::DEADLOCK) {
      sched.GetStrategy().ResetCurrentRound();
    } else {
      sched.ReplayRound(
          RoundMinimizorT<Event>::GetTasksOrdering(
              nonlinear_history.full, std::unordered_set<int>{}),
          ReplayMode::CompleteOnLast);
    }
  }

 protected:
  /**
   * This method is called when the minimizor tries to remove tasks from the
   * non-linearizable history. Greedy minimizors call it for all task ids `i` and
   * for all pairs of task ids `(i, j)`, where `i < j`.
   *
   * @param sched the scheduler that will replay the round with some tasks
   * marked as removed.
   * @param nonlinear_history the non-linearizable history being minimized.
   * @param task_ids the task ids that the minimizor tries to remove.
   * @return the new non-linearizable history if the tasks were successfully
   * removed, `std::nullopt` otherwise.
   */
  virtual ResultT OnTasksRemoved(
      SchedulerT& sched,
      const NonLinearizableHistoryT& nonlinear_history,
      const std::unordered_set<int>& task_ids) const = 0;
};

/**
 * `SameInterleavingMinimizorT` uses the provided non-linearizable history and
 * removes selected tasks from the full and sequential histories without
 * changing the rest of the interleaving.
 *
 * If the initial full history looked like this `[1, 1, 2, 3, 2, 2, 1, 3]`
 * (task ids), and we remove task with id 2, the new full history will be
 * `[1, 1, 3, 1, 3]`.
 */
template <class Event>
struct SameInterleavingMinimizorT : public GreedyRoundMinimizorT<Event> {
  using Base = GreedyRoundMinimizorT<Event>;
  using SchedulerT = typename Base::SchedulerT;
  using NonLinearizableHistoryT = typename Base::NonLinearizableHistoryT;
  using ResultT = typename Base::ResultT;

 protected:
  ResultT OnTasksRemoved(
      SchedulerT& sched,
      const NonLinearizableHistoryT& nonlinear_history,
      const std::unordered_set<int>& task_ids) const override {
    auto new_ordering =
        RoundMinimizorT<Event>::GetTasksOrdering(nonlinear_history.full, task_ids);

    // Preserve deadlocks during replay
    const ReplayMode mode =
        (nonlinear_history.reason == NonLinearizableHistoryT::Reason::DEADLOCK)
            ? ReplayMode::NoForceComplete
            : ReplayMode::CompleteOnLast;

    return sched.ReplayRound(new_ordering, mode);
  }
};

/**
 * `StrategyExplorationMinimizorT` tries to remove tasks from the
 * non-linearizable history and then explores different interleavings of the
 * current round.
 *
 * It uses the `runs` parameter to determine how many times it should try to
 * find a new non-linearizable history with the provided tasks marked as
 * removed. If such history is not found in a given number of runs, the tasks
 * are returned to their initial non-removed state.
 */
template <class Event>
struct StrategyExplorationMinimizorT : public GreedyRoundMinimizorT<Event> {
  using Base = GreedyRoundMinimizorT<Event>;
  using SchedulerT = typename Base::SchedulerT;
  using NonLinearizableHistoryT = typename Base::NonLinearizableHistoryT;
  using ResultT = typename Base::ResultT;

  StrategyExplorationMinimizorT() = delete;
  explicit StrategyExplorationMinimizorT(int runs_) : runs(runs_) {}

 protected:
  ResultT OnTasksRemoved(
      SchedulerT& sched,
      const NonLinearizableHistoryT& /*nonlinear_history*/,
      const std::unordered_set<int>& task_ids) const override {
    auto mark = [&](bool is_removed) {
      for (int id : task_ids) {
        sched.GetStrategy().SetTaskRemoved(id, is_removed);
      }
    };

    mark(true);
    ResultT new_histories = sched.ExploreRound(runs);
    if (!new_histories.has_value()) {
      mark(false);
    }
    return new_histories;
  }

 private:
  int runs;
};

// ----------------------------
// Backward-compatible aliases (normal + dual)
// ----------------------------

// normal (HistoryEvent)
using RoundMinimizor = RoundMinimizorT<HistoryEvent>;
using GreedyRoundMinimizor = GreedyRoundMinimizorT<HistoryEvent>;
using SameInterleavingMinimizor = SameInterleavingMinimizorT<HistoryEvent>;
using StrategyExplorationMinimizor = StrategyExplorationMinimizorT<HistoryEvent>;

// dual (DualHistoryEvent)
using DualRoundMinimizor = RoundMinimizorT<DualHistoryEvent>;
using DualSameInterleavingMinimizor = SameInterleavingMinimizorT<DualHistoryEvent>;
using DualStrategyExplorationMinimizor = StrategyExplorationMinimizorT<DualHistoryEvent>;

}  // namespace ltest
