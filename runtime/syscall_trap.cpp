#include "syscall_trap.h"

/// Required for incapsulating syscall traps only in special places where it's
/// really needed
bool __trap_syscall = 0;

ltest::SyscallTrapGuard::SyscallTrapGuard() { __trap_syscall = true; }

ltest::SyscallTrapGuard::~SyscallTrapGuard() { __trap_syscall = false; }
