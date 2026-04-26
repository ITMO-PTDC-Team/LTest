#include <dlfcn.h>
#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <syscall.h>

#include <cerrno>
#include <cstring>

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
  // to avoid infinity catches
  ltest_coro_ctx = false;
  int res;
  switch (syscall_number) {
    case SYS_sched_yield:
      res = ltest_sched_yield(result);
      break;
    case SYS_futex:
      res = ltest_futex(arg0, arg1, arg2, result);
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

static void *(*real_calloc)(size_t nmemb, size_t size);
static void *(*real_malloc)(size_t size);
static void (*real_free)(void *ptr);
static void *(*real_realloc)(void *ptr, size_t size);

void *malloc(size_t size) {
  // //write(2, "malloc\n", 7);
  if (!real_malloc) {
    reinterpret_cast<void *&>(real_malloc) = dlsym(RTLD_NEXT, "malloc");
  }
  void *p = real_malloc(size);
  if (ltest_coro_ctx) {
    ltest::SchedCtxGuard guard;
    memory_handler->RememberPointer(p);
  }
  return p;
}

void free(void *ptr) {
  // write(2, "free\n", 5);
  if (!real_free) {
    reinterpret_cast<void *&>(real_free) = dlsym(RTLD_NEXT, "free");
  }
  if (ltest_coro_ctx && ptr != nullptr) {
    ltest::SchedCtxGuard guard;
    memory_handler->ForgetAboutPointer(ptr);
  }
  real_free(ptr);
}

void *calloc(size_t nmemb, size_t size) {
  // write(2, "calloc\n", 7);
  if (!real_calloc) {
    reinterpret_cast<void *&>(real_calloc) = dlsym(RTLD_NEXT, "calloc");
  }
  void *p = real_calloc(nmemb, size);
  if (ltest_coro_ctx) {
    ltest::SchedCtxGuard guard;
    memory_handler->RememberPointer(p);
  }
  return p;
}

void *realloc(void *ptr, size_t size) {
  // write(2, "realloc\n", 8);
  if (!real_realloc) {
    reinterpret_cast<void *&>(real_realloc) = dlsym(RTLD_NEXT, "realloc");
  }
  void *p = real_realloc(ptr, size);
  if (ltest_coro_ctx && p != ptr && p != nullptr) {
    ltest::SchedCtxGuard guard;
    memory_handler->RememberPointer(p);
    memory_handler->ForgetAboutPointer(ptr);
  }
  return p;
}