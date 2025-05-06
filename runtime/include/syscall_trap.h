#pragma once

extern bool ltest_trap_syscall;

namespace ltest {

struct SyscallTrapGuard {
  SyscallTrapGuard();
  ~SyscallTrapGuard();
};

}  // namespace ltest