#include "include/scheduler.h"

namespace {
Strategy* g_active_strategy = nullptr;
}

void SetActiveStrategy(Strategy* strategy) {
  // store pointer to current strategy for WMM choice callbacks
  g_active_strategy = strategy;
}

Strategy* GetActiveStrategy() {
  return g_active_strategy;
}
