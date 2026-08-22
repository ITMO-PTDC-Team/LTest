#pragma once

#include <vector>

#include "lib.h"

namespace ltest {

struct CustomRound {
  CustomRound(std::vector<std::vector<TaskBuilder>> threads_)
      : threads(threads_) {}

  std::vector<std::vector<TaskBuilder>> threads;
};

}  // namespace ltest
