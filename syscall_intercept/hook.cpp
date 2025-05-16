#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <sys/syscall.h>
#include <syscall.h>

#include <cerrno>

#include "runtime/include/block_manager.h"
#include "runtime/include/coro_ctx_guard.h"
#include "runtime/include/lib.h"
#include "runtime/include/logger.h"

static int ltest_sched_yield(long *result) {
  debug(stderr, "caught sched_yield()\n");
  CoroYield();
  *result = 0;
  return 0;
}

static int ltest_futex(long arg0, long arg1, long arg2, long *result) {
  debug(stderr, "caught futex(0x%lx, %ld), exp: %ld, cur: %d\n",
        (unsigned long)arg0, arg1, arg2, *((int *)arg0));
  arg1 = arg1 & FUTEX_CMD_MASK;
  if (arg1 == FUTEX_WAIT || arg1 == FUTEX_WAIT_BITSET) {
    auto fstate = BlockState{arg0, arg2};
    if (fstate.CanBeBlocked()) {
      this_coro->SetBlocked(fstate);
      CoroYield();
      *result = 0;
    } else {
      errno = EAGAIN;
      *result = 1;
    }
  } else if (arg1 == FUTEX_WAKE || arg1 == FUTEX_WAKE_BITSET) {
    debug(stderr, "caught wake\n");
    *result = block_manager.UnblockOn(arg0, arg2);
  } else {
    assert(false && "unsupported futex call");
  }
  return 0;
}

static int hook(long syscall_number, long arg0, long arg1, long arg2, long arg3,
                long arg4, long arg5, long *result) {
  if (!ltest_coro_ctx) {
    return 1;
  }
  switch (syscall_number) {
    case SYS_sched_yield:
      return ltest_sched_yield(result);
    case SYS_futex:
      return ltest_futex(arg0, arg1, arg2, result);
    default:
      return 1;
  }
}

static __attribute__((constructor)) void init(void) {
  // Set up the callback function
  intercept_hook_point = hook;
}