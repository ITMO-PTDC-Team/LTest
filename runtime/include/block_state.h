#pragma once

#include <cstdint>
struct BlockState {
  std::intptr_t addr;
  long value;

  inline bool CanBeBlocked() { return *reinterpret_cast<int *>(addr) == value; }
};