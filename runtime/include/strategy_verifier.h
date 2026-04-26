#pragma once
#include "scheduler.h"

struct DefaultStrategyTaskVerifier {
  bool Verify(const string& name, size_t thread_id, bool is_new) { return true; }

  void OnFinished(Task& task, size_t thread_id) {}

  void Reset() {}
};
