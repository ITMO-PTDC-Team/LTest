#include "syscall_trap.h"

/// Required for incapsulating syscall traps only in special places where it's
/// really needed
bool ltest_trap_syscall = 0;

ltest::SyscallTrapGuard::SyscallTrapGuard() { ltest_trap_syscall = true; }

ltest::SyscallTrapGuard::~SyscallTrapGuard() { ltest_trap_syscall = false; }
