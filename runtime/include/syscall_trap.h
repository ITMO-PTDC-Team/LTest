#pragma once

extern bool __trap_syscall;  // NOLINT(readability-identifier-naming)

namespace ltest {

struct SyscallTrapGuard {
  SyscallTrapGuard();
  ~SyscallTrapGuard();
};

}  // namespace ltest