#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <sys/syscall.h>
#include <syscall.h>
#include "runtime/include/logger.h"
#include "runtime/include/lib.h"
#include "runtime/include/syscall_trap.h"

static int
hook(long syscall_number,
			long arg0, long arg1,
			long arg2, long arg3,
			long arg4, long arg5,
			long *result)
{
	if (!__trap_syscall) {
		return 1;
	}
	if (syscall_number == SYS_sched_yield) {
		debug(stderr, "caught sched_yield()\n");
		CoroYield();
		*result = 0;
		return 0;
	} else if (syscall_number == SYS_futex) {
		debug(stderr, "caught futex(0x%lx, %ld, %ld)\n", (unsigned long)arg0, arg1, arg2);
		if (arg1 == FUTEX_WAIT_PRIVATE || arg1 == FUTEX_WAIT_BITSET_PRIVATE) {
			this_coro->SetBlocked(arg0, arg2);
		} else if (arg1 == FUTEX_WAKE_PRIVATE || arg1 == FUTEX_WAKE_BITSET_PRIVATE) {
			debug(stderr, "caught wake\n");
		} else {
			assert(false && "unsupported futex call");
		}
		CoroYield();
		*result = 0;
		return 0;
	} else {
		/*
		 * Ignore any other syscalls
		 * i.e.: pass them on to the kernel
		 * as would normally happen.
		 */
		return 1;
	}
}

static __attribute__((constructor)) void
init(void)
{
	// Set up the callback function
	intercept_hook_point = hook;
}