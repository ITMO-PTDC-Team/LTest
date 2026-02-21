#pragma once
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include "lincheck.h"
#include "lincheck_dual.h"

struct Strategy;
struct RoundMinimizor;

// ------------------------------
// Generic scheduler interfaces
// ------------------------------
template <class Event>
struct BasicScheduler {
  using event_t = Event;

  using FullHistory = std::vector<std::reference_wrapper<Task>>;
  using SeqHistory = std::vector<Event>;

  struct NonLinearizableHistory {
    enum class Reason { DEADLOCK, NON_LINEARIZABLE_HISTORY };

    FullHistory full;
    SeqHistory seq;
    Reason reason;
  };

  using Result = std::optional<NonLinearizableHistory>;

  virtual Result Run() = 0;
  virtual ~BasicScheduler() = default;
};

template <class Event>
struct BasicSchedulerWithReplay : BasicScheduler<Event> {
 protected:
  // Friend declarations are ok even if classes are defined later;
  // this keeps existing minimization code working.
  friend class GreedyRoundMinimizor;
  friend class SameInterleavingMinimizor;
  friend class StrategyExplorationMinimizor;
  friend class SmartMinimizor;

  using Base = BasicScheduler<Event>;
  using Result = typename Base::Result;
  using NonLinearizableHistory = typename Base::NonLinearizableHistory;

  virtual Result RunRound() = 0;
  virtual Result ExploreRound(int runs) = 0;
  virtual Result ReplayRound(const std::vector<int>& tasks_ordering) = 0;
  virtual Strategy& GetStrategy() const = 0;
  virtual void Minimize(NonLinearizableHistory& nonlinear_history,
                        const RoundMinimizor& minimizor) = 0;
};

// ------------------------------
// Backward-compatible aliases
// ------------------------------
using Scheduler = BasicScheduler<HistoryEvent>;
using SchedulerWithReplay = BasicSchedulerWithReplay<HistoryEvent>;

// Dual-mode types:
using DualScheduler = BasicScheduler<DualHistoryEvent>;

using DualSchedulerWithReplay = BasicSchedulerWithReplay<DualHistoryEvent>;