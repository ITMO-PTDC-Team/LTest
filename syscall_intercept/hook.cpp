#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <sys/mman.h>
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

static int ltest_mmap(long arg0, long i, long arg2, long arg3, long arg4, long arg5, long *result) {
  // we need sadly here pass parameters as is to mmap to handle flags correctly
  void* ptr = mmap(reinterpret_cast<void*>(arg0), i, arg2, arg3, arg4, arg5);
  memory_handler->RememberRawPtr(ptr, i);
  *result = reinterpret_cast<long>(ptr);
  return 0;

}

int ltest_munmap(long arg0, long arg1, long* result) {
  *result = 0;
  memory_handler->DeleteRawPtr(reinterpret_cast<void*>(arg0), arg1);
  return 0;
}
static int hook(long syscall_number, long arg0, long arg1, long arg2, long arg3,
                long arg4, long arg5, long *result) {
  if (!ltest_coro_ctx) {
    return 1;
  }
  //to avoid infinity catches
  ltest_coro_ctx = false;
  int res;
  switch (syscall_number) {
    case SYS_sched_yield:
      res = ltest_sched_yield(result);
      break;
    case SYS_futex:
      res = ltest_futex(arg0, arg1, arg2, result);
      break;
    case SYS_mmap:
      res = ltest_mmap(arg0, arg1, arg2, arg3, arg4, arg5, result);
      break;
    case SYS_munmap:
      res = ltest_munmap(arg0, arg1, result);
      break;
    default:
      res = 1;
  }

  ltest_coro_ctx = true;
  return res;
}

static __attribute__((constructor)) void init(void) {
  // Set up the callback function
  intercept_hook_point = hook;
}