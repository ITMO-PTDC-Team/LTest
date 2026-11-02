#pragma once

#include <cstdint>

namespace ltest {

struct BlockState {
  std::intptr_t addr;  // The address of the tracking variable
  long value;

  // Futex waits compare only the low 32 bits of both operands.
  bool CanBeBlocked() const {
    return *reinterpret_cast<const std::uint32_t*>(addr) ==
           static_cast<std::uint32_t>(value);
  }
};

}  // namespace ltest
