#pragma once
#include "scheduler.h"

struct DefaultStrategyVerifier {
  inline bool Verify(CreatedTaskMetaData task) { return true; }

  inline void OnFinished(TaskWithMetaData task) {}

  inline std::optional<std::string> ReleaseTask(size_t thread_id) {
    return std::nullopt;
  }
};
