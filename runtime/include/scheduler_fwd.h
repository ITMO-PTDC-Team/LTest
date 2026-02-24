#pragma once
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include "lincheck.h"
#include "lincheck_dual.h"

struct Strategy;

// NEW: replay mode (to preserve deadlocks during replay)
enum class ReplayMode {
  // Old behavior: on the last appearance of a task id we force-complete it via Terminate()
  CompleteOnLast,

  // Deadlock-friendly: never force-complete tasks (never call Terminate()).
  // Used to replay schedules without destroying deadlocks.
  NoForceComplete,
};

// Forward declarations for templated minimization (header-only plan)
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
  virtual Result ExploreRound(int runs) = 0;

  // CHANGED: ReplayRound has a mode; default keeps old behavior.
  virtual Result ReplayRound(const std::vector<int>& tasks_ordering,
                             ReplayMode mode = ReplayMode::CompleteOnLast) = 0;

  virtual Strategy& GetStrategy() const = 0;

  virtual void Minimize(NonLinearizableHistory& nonlinear_history,
                        const RoundMinimizorT<Event>& minimizor) = 0;
};

// ------------------------------
// Backward-compatible aliases
// ------------------------------
using Scheduler = BasicScheduler<HistoryEvent>;
using SchedulerWithReplay = BasicSchedulerWithReplay<HistoryEvent>;

// Dual-mode types:
using DualScheduler = BasicScheduler<DualHistoryEvent>;
using DualSchedulerWithReplay = BasicSchedulerWithReplay<DualHistoryEvent>;