#pragma once

extern bool __yield;

namespace ltest {

struct AllowYieldArea {
  AllowYieldArea();
  ~AllowYieldArea();
};

}  // namespace ltest