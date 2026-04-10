#pragma once
#include "scheduler.h"

struct DefaultStrategyTaskVerifier {
  inline bool Verify(const string& name, size_t thread_id) { return true; }

  inline void OnFinished(Task& task, size_t thread_id) {}

  inline void Reset() {}
};
