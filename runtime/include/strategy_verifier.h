#pragma once
#include "scheduler.h"

struct DefaultStrategyVerifier {
  inline bool Verify(CreatedTaskMetaData task) { return true; }

  inline void OnFinished(TaskWithMetaData task) {}

  inline void Reset() {}
};
