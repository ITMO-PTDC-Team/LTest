#pragma once
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include "lincheck.h"
#include "lincheck_dual.h"

namespace ltest {

struct Strategy;

enum class ReplayMode {
  CompleteOnLast,
  NoForceComplete,
};

enum class DeadlockPolicy {
  // Report a visible deadlock as a failure.
  Fail,

  // On deadlock, check the partial history and stop the round if it is
  // linearizable.
  Check,

  // Check the partial history and explore nearby schedules if it is
  // linearizable.
  Explore,

  // Directed dual search: after a linearizable deadlock, remove one blocked
  // wait operation, append a user-declared progress operation, and replay.
  Rollback,
};

template <class Event>
struct RoundMinimizorT;

template <class Event>
struct GreedyRoundMinimizorT;

template <class Event>
struct SameInterleavingMinimizorT;

template <class Event>
struct StrategyExplorationMinimizorT;

template <class Event>
struct SmartMinimizorT;

template <class Event>
struct BasicScheduler {
  using event_t = Event;

  using FullHistory = std::vector<std::reference_wrapper<Task>>;
  using SeqHistory = std::vector<Event>;

  struct NonLinearizableHistory {
    enum class Reason { ASSERT_FAILURE, DEADLOCK, NON_LINEARIZABLE_HISTORY };
    FullHistory full;
    SeqHistory seq;
    Reason reason;
  };

  using Result = std::optional<NonLinearizableHistory>;

  virtual Result Run() = 0;

  virtual size_t GetStrategyThreadsCount() const = 0;

  virtual ~BasicScheduler() = default;
};

template <class Event>
struct BasicSchedulerWithReplay : BasicScheduler<Event> {
 protected:
  template <class>
  friend struct GreedyRoundMinimizorT;
  template <class>
  friend struct SameInterleavingMinimizorT;
  template <class>
  friend struct StrategyExplorationMinimizorT;
  template <class>
  friend struct SmartMinimizorT;

  using Base = BasicScheduler<Event>;
  using Result = typename Base::Result;
  using NonLinearizableHistory = typename Base::NonLinearizableHistory;

  virtual Result RunRound() = 0;

  virtual Result ExploreRound(int runs,
                              bool log_each_interleaving = false) = 0;

  virtual Result ReplayRound(
      const std::vector<int>& tasks_ordering,
      ReplayMode mode = ReplayMode::CompleteOnLast) = 0;

  [[nodiscard]] virtual Strategy& GetStrategy() const = 0;

  virtual void Minimize(NonLinearizableHistory& nonlinear_history,
                        const RoundMinimizorT<Event>& minimizor) = 0;
};

using Scheduler = BasicScheduler<HistoryEvent>;
using SchedulerWithReplay = BasicSchedulerWithReplay<HistoryEvent>;

using DualScheduler = BasicScheduler<DualHistoryEvent>;
using DualSchedulerWithReplay = BasicSchedulerWithReplay<DualHistoryEvent>;
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
