#pragma once

extern bool ltest_yield;

namespace ltest {

struct AllowYieldArea {
  AllowYieldArea();
  ~AllowYieldArea();
};

}  // namespace ltest