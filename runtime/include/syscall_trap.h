#pragma once

extern bool __trap_syscall;

namespace ltest {

struct SyscallTrapGuard {
  SyscallTrapGuard();
  ~SyscallTrapGuard();
};

}  // namespace ltest