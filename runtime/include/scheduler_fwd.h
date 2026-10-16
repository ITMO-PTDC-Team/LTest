#pragma once

#include "lincheck.h"
#include "lincheck_dual.h"

namespace ltest {

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
struct BasicScheduler {
  using event_t = Event;

  virtual void Run() = 0;
  virtual size_t GetStrategyThreadsCount() const = 0;
  virtual DeadlockPolicy GetDeadlockPolicy() const = 0;

  virtual ~BasicScheduler() = default;
};

using Scheduler = BasicScheduler<HistoryEvent>;

using DualScheduler = BasicScheduler<DualHistoryEvent>;

}  // namespace ltest
