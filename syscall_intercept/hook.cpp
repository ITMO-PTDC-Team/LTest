#include <dlfcn.h>
#include <fcntl.h>
#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <poll.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/syscall.h>

#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <ctime>

#include "runtime/include/block_manager.h"
#include "runtime/include/coro_ctx_guard.h"
#include "runtime/include/lib.h"
#include "runtime/include/logger.h"
#include "runtime/include/verifying_macro.h"
#include "runtime/io.h"

namespace io = ltest::io;

static int ltest_sched_yield(long *result) {
  debug(stderr, "caught sched_yield()\n");
  // otherwise here will  be a infinity loop
  ltest::CoroCtxGuard guard;
  CoroYield();
  *result = 0;
  return 0;
}

static int ltest_futex(long arg0, long arg1, long arg2, long *result) {
  debug(stderr, "caught futex(0x%lx, %ld), exp: %ld, cur: %d\n",
        (unsigned long)arg0, arg1, arg2, *((int *)arg0));
  arg1 = arg1 & FUTEX_CMD_MASK;
  if (arg1 == FUTEX_WAIT || arg1 == FUTEX_WAIT_BITSET) {
    auto fstate = ltest::BlockState{arg0, arg2};
    if (fstate.CanBeBlocked()) {
      ltest::this_coro->SetBlocked(fstate);
      // otherwise here will  be a infinity loop
      ltest::CoroCtxGuard guard;
      CoroYield();
      *result = 0;
    } else {
      *result = -EAGAIN;
    }
  } else if (arg1 == FUTEX_WAKE || arg1 == FUTEX_WAKE_BITSET) {
    debug(stderr, "caught wake\n");
    *result = ltest::block_manager.UnblockOn(arg0, arg2);
  } else {
    assert(false && "unsupported futex call");
  }
  return 0;
}

static long ltest_dup(long number, int fd, long argument, long flags) {
  auto file = io::TrackFile(fd);
  const long result = syscall_no_intercept(number, fd, argument, flags);
  if (result >= 0 && result != fd) {
    io::UnregisterFile(result);
    io::RegisterFile(result, file);
  }
  return result;
}

static void MarkTransferredFiles(const msghdr *message) {
  if (!message) return;
  for (auto *control = CMSG_FIRSTHDR(message); control;
       control = CMSG_NXTHDR(const_cast<msghdr *>(message), control)) {
    const size_t offset = reinterpret_cast<const char *>(control) -
                          static_cast<const char *>(message->msg_control);
    if (control->cmsg_len < CMSG_LEN(0) ||
        control->cmsg_len > message->msg_controllen - offset)
      break;
    if (control->cmsg_level != SOL_SOCKET || control->cmsg_type != SCM_RIGHTS)
      continue;
    const size_t count = (control->cmsg_len - CMSG_LEN(0)) / sizeof(int);
    const auto *descriptors = reinterpret_cast<const int *>(CMSG_DATA(control));
    for (size_t i = 0; i < count; ++i)
      if (auto file = io::FindFile(descriptors[i])) file->MarkExternal();
  }
}

static int hook(long syscall_number, long arg0, long arg1, long arg2, long arg3,
                long arg4, long arg5, long *result) {
  if (!ltest::ltest_coro_ctx) {
    return 1;
  }
  // to avoid allocation mismatches
  ltest::SchedCtxGuard guard;
  switch (syscall_number) {
    case SYS_sched_yield:
      return ltest_sched_yield(result);
    case SYS_futex:
      return ltest_futex(arg0, arg1, arg2, result);
    case SYS_epoll_create:
    case SYS_epoll_create1:
      *result = syscall_no_intercept(syscall_number, arg0);
      if (*result >= 0) io::RegisterEpoll(*result);
      break;
    case SYS_epoll_ctl:
      *result = io::ControlEpoll(arg0, arg1, arg2,
                                 reinterpret_cast<const epoll_event *>(arg3));
      break;
    case SYS_epoll_pwait2: {
      const auto *timeout = reinterpret_cast<const timespec *>(arg3);
      if (timeout && (timeout->tv_sec < 0 || timeout->tv_nsec < 0 ||
                      timeout->tv_nsec >= 1000000000)) {
        *result = -EINVAL;
      } else if (arg4) {
        *result = -EOPNOTSUPP;
      } else {
        *result = io::WaitEpoll(
            arg0, reinterpret_cast<epoll_event *>(arg1), arg2,
            timeout && timeout->tv_sec == 0 && timeout->tv_nsec == 0);
      }
      break;
    }
    case SYS_epoll_pwait:
    case SYS_epoll_wait:
      *result = syscall_number == SYS_epoll_pwait && arg4
                    ? -EOPNOTSUPP
                    : io::WaitEpoll(arg0, reinterpret_cast<epoll_event *>(arg1),
                                    arg2, static_cast<int>(arg3) == 0);
      break;
    case SYS_eventfd:
    case SYS_eventfd2: {
      const int flags = syscall_number == SYS_eventfd2 ? arg1 : 0;
      *result = syscall_no_intercept(SYS_eventfd2, arg0, flags | EFD_NONBLOCK);
      if (*result >= 0) io::RegisterEventFd(*result, arg0, flags);
      break;
    }
    case SYS_pipe:
    case SYS_pipe2: {
      auto *ends = reinterpret_cast<int *>(arg0);
      const int flags = syscall_number == SYS_pipe2 ? arg1 : 0;
      *result = syscall_no_intercept(SYS_pipe2, ends, flags | O_NONBLOCK);
      if (*result == 0) io::RegisterPipe(ends[0], ends[1], flags);
      break;
    }
    case SYS_read:
    case SYS_write: {
      auto file = io::FindFile(arg0);
      if (!file || !file->nonblocking) return 1;
      *result = file->ReadWrite(syscall_number == SYS_write,
                                reinterpret_cast<void *>(arg1), arg2);
      break;
    }
    case SYS_close:
      *result = syscall_no_intercept(SYS_close, arg0);
      // Linux releases the descriptor before reporting delayed close errors.
      if (*result != -EBADF) io::UnregisterFile(arg0);
      break;
    case SYS_ioctl: {
      auto file = io::FindFile(arg0);
      if (!file || !file->nonblocking || arg1 != FIONBIO) return 1;
      if (!arg2) {
        *result = -EFAULT;
        break;
      }
      const bool nonblocking = *reinterpret_cast<const int *>(arg2) != 0;
      int enabled = 1;
      *result = syscall_no_intercept(SYS_ioctl, arg0, arg1, &enabled);
      if (*result == 0) file->nonblocking = nonblocking;
      break;
    }
    case SYS_dup:
    case SYS_dup2:
    case SYS_dup3:
      *result = ltest_dup(syscall_number, arg0, arg1, arg2);
      break;
    case SYS_fcntl: {
      if (arg1 == F_DUPFD || arg1 == F_DUPFD_CLOEXEC) {
        *result = ltest_dup(SYS_fcntl, arg0, arg1, arg2);
        break;
      }
      auto file = io::FindFile(arg0);
      if (!file || !file->nonblocking) return 1;
      if (arg1 == F_GETFL) {
        *result = syscall_no_intercept(SYS_fcntl, arg0, arg1, arg2);
        if (*result >= 0) {
          *result &= ~O_NONBLOCK;
          if (*file->nonblocking) *result |= O_NONBLOCK;
        }
      } else if (arg1 == F_SETFL) {
        *result =
            syscall_no_intercept(SYS_fcntl, arg0, arg1, arg2 | O_NONBLOCK);
        if (*result == 0) file->nonblocking = arg2 & O_NONBLOCK;
      } else {
        return 1;
      }
      break;
    }
    case SYS_poll:
    case SYS_ppoll: {
      const auto *descriptors = reinterpret_cast<const pollfd *>(arg0);
      const auto *timeout = reinterpret_cast<const timespec *>(arg2);
      const bool immediate =
          syscall_number == SYS_poll
              ? static_cast<int>(arg2) == 0
              : timeout && timeout->tv_sec == 0 && timeout->tv_nsec == 0;
      if (!descriptors || immediate) return 1;
      for (long i = 0; i < arg1; ++i) {
        auto file = io::FindFile(descriptors[i].fd);
        if (file && (file->nonblocking || !file->external)) {
          *result = -EOPNOTSUPP;
          return 0;
        }
      }
      return 1;
    }
    case SYS_sendmsg:
      *result = syscall_no_intercept(SYS_sendmsg, arg0, arg1, arg2);
      if (*result >= 0)
        MarkTransferredFiles(reinterpret_cast<const msghdr *>(arg1));
      break;
    case SYS_sendmmsg: {
      *result = syscall_no_intercept(SYS_sendmmsg, arg0, arg1, arg2, arg3);
      const auto *messages = reinterpret_cast<const mmsghdr *>(arg1);
      for (long i = 0; i < *result; ++i)
        MarkTransferredFiles(&messages[i].msg_hdr);
      break;
    }
    case SYS_fork:
    case SYS_vfork:
    case SYS_clone:
    case SYS_clone3:
      io::MarkFilesExternal();
      return 1;
    default:
      return 1;
  }
  return 0;
}

static __attribute__((constructor)) void init(void) {
  // Set up the callback function
  intercept_hook_point = hook;
}
extern "C" {
void __assert_fail(const char *assertion, const char *file, unsigned int line,
                   const char *function) {
  if (ltest::ltest_coro_ctx) {
    {
      ltest::SchedCtxGuard guard;
      ltest::SetTestFailure(
          ltest::FormatTestFailure(assertion, file, line, function));
    }
    CoroYield();
    {
      ltest::SchedCtxGuard guard;
      assert("after triggered failed assert, the test should be finished");
    }
  }

  static void (*real_assert)(const char *assertion, const char *file,
                             unsigned int line, const char *function);
  if (!real_assert) {
    reinterpret_cast<void *&>(real_assert) = dlsym(RTLD_NEXT, "__assert_fail");
  }
  real_assert(assertion, file, line, function);
}
}
