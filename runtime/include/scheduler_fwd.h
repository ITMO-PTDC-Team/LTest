#pragma once
#include <optional>
#include <variant>
#include <vector>

#include "lincheck.h"

struct Strategy;
struct RoundMinimizor;

struct Scheduler {
  using FullHistory = std::vector<std::reference_wrapper<Task>>;
  using SeqHistory = std::vector<std::variant<Invoke, Response>>;

  struct NonLinearizableHistory {
    enum class Reason { DEADLOCK, NON_LINEARIZABLE_HISTORY };
    FullHistory full;
    SeqHistory seq;
    Reason reason;
  };

  using Result = std::optional<NonLinearizableHistory>;

  virtual Result Run() = 0;

  virtual ~Scheduler() = default;
};

struct SchedulerWithReplay : Scheduler {
 protected:
  friend class GreedyRoundMinimizor;
  friend class SameInterleavingMinimizor;
  friend class StrategyExplorationMinimizor;
  friend class SmartMinimizor;

  virtual Result RunRound() = 0;

  virtual Result ExploreRound(int runs) = 0;

  virtual Result ReplayRound(const std::vector<int>& tasks_ordering) = 0;

  virtual Strategy& GetStrategy() const = 0;

  virtual void Minimize(NonLinearizableHistory& nonlinear_history,
                        const RoundMinimizor& minimizor) = 0;
};