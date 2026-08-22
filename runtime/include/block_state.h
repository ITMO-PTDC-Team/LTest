#pragma once

#include <cstdint>
namespace ltest {

struct BlockState {
  std::intptr_t addr; // The address of the tracking variable
  long value;

  // That is, a mutex is free if and only if it is ex qual to the current state value == address.
  inline bool CanBeBlocked() { return *reinterpret_cast<uint32_t *>(addr) == value; }
};

}  // namespace ltest
