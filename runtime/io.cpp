#include "io.h"

#include <fcntl.h>
#include <libsyscall_intercept_hook_point.h>
#include <poll.h>
#include <sys/eventfd.h>
#include <sys/syscall.h>

#include <algorithm>
#include <cassert>
#include <cerrno>
#include <cstring>
#include <map>
#include <vector>

#include "include/coro_ctx_guard.h"
#include "include/io_wait.h"
#include "include/lib.h"

namespace ltest {
namespace io {
namespace {

constexpr uint64_t kMaxCounter = UINT64_MAX - 1;

struct IoWaiter {
  const CoroBase* task;
  std::shared_ptr<File> file;
  short events;
  uint64_t value;
  int* error;
};

// All identities and waiters belong to one forked attempt. No private fd
// duplicates keep kernel objects alive after their last target close.
std::map<int, std::shared_ptr<File>> files;
std::vector<IoWaiter> waiters;

struct EventFd final : File {
  EventFd(unsigned value, int flags) : counter(value) {
    external = false;
    nonblocking = flags & EFD_NONBLOCK;
  }

  bool Ready(short events, uint64_t value) const override {
    if (events == POLLOUT && !external) return value <= kMaxCounter - counter;
    return File::Ready(events, value);
  }

  long ReadWrite(bool write, void* buffer, size_t size) override {
    uint64_t pending_value = 0;
    bool captured = false;
    for (;;) {
      void* data = captured ? &pending_value : buffer;
      const long result = syscall_no_intercept(write ? SYS_write : SYS_read,
                                               *aliases.begin(), data, size);
      if (result >= 0) {
        uint64_t value;
        std::memcpy(&value, data, sizeof(value));
        // Once shared, foreign operations can change the kernel counter.
        if (!external) {
          if (write)
            counter += value;
          else
            counter -= value;
        }
        return result;
      }
      if (result != -EAGAIN || *nonblocking) return result;
      if (write && !captured) {
        // A blocking kernel eventfd write copies its value before sleeping.
        std::memcpy(&pending_value, buffer, sizeof(pending_value));
        captured = true;
      }
      const int error =
          Park(shared_from_this(), write ? POLLOUT : POLLIN, pending_value);
      if (error) return error;
    }
  }

  uint64_t counter;
};

struct PipeEnd final : File {
  explicit PipeEnd(int flags) {
    nonblocking = flags & O_NONBLOCK;
    // O_NOTIFICATION_PIPE uses O_EXCL; those pipes have kernel producers.
    external = flags & O_EXCL;
  }

  void MarkExternal() override {
    external = true;
    if (auto other = peer.lock()) other->external = true;
  }

  std::weak_ptr<File> peer;
};

bool CanContinue(const IoWaiter& waiter) {
  if (waiter.file->aliases.empty()) {
    *waiter.error = -EBADF;
    return true;
  }
  return waiter.file->Ready(waiter.events, waiter.value);
}

bool PollIoWaiters() { return std::erase_if(waiters, CanContinue) != 0; }

std::vector<pollfd> ExternalSources(const CoroBase* task = nullptr) {
  std::map<int, short> sources;
  for (const auto& waiter : waiters) {
    if (task && waiter.task != task) continue;
    if (waiter.file->CanMakeExternalProgress())
      sources[*waiter.file->aliases.begin()] |= waiter.events;
  }
  std::vector<pollfd> descriptors;
  for (const auto& [fd, events] : sources)
    descriptors.push_back({fd, events, 0});
  return descriptors;
}

}  // namespace

std::shared_ptr<File> FindFile(int fd) {
  auto it = files.find(fd);
  return it == files.end() ? nullptr : it->second;
}

std::shared_ptr<File> TrackFile(int fd) {
  if (auto file = FindFile(fd)) return file;
  if (syscall_no_intercept(SYS_fcntl, fd, F_GETFD, 0) < 0) return {};
  auto file = std::make_shared<File>();
  RegisterFile(fd, file);
  return file;
}

void RegisterFile(int fd, const std::shared_ptr<File>& file) {
  file->aliases.insert(fd);
  files.emplace(fd, file);
}

void UnregisterFile(int fd) {
  auto file = FindFile(fd);
  if (!file) return;
  files.erase(fd);
  file->aliases.erase(fd);
  if (!file->aliases.empty()) return;
  file->Forget(file.get());
  for (const auto& entry : files) entry.second->Forget(file.get());
}

void RegisterEventFd(int fd, unsigned value, int flags) {
  RegisterFile(fd, std::make_shared<EventFd>(value, flags));
}

void RegisterPipe(int read_fd, int write_fd, int flags) {
  auto reader = std::make_shared<PipeEnd>(flags);
  auto writer = std::make_shared<PipeEnd>(flags);
  reader->peer = writer;
  writer->peer = reader;
  RegisterFile(read_fd, reader);
  RegisterFile(write_fd, writer);
}

void MarkFilesExternal() {
  for (const auto& entry : files) entry.second->MarkExternal();
}

bool File::CanMakeExternalProgress() const {
  return external && !aliases.empty();
}

bool File::Ready(short events, uint64_t) const {
  pollfd fd{*aliases.begin(), events, 0};
  const long result = syscall_no_intercept(SYS_poll, &fd, 1, 0);
  return result > 0 && fd.revents != 0;
}

long File::ReadWrite(bool write, void* buffer, size_t size) {
  size_t written = 0;
  for (;;) {
    void* data = written ? static_cast<char*>(buffer) + written : buffer;
    const long result = syscall_no_intercept(
        write ? SYS_write : SYS_read, *aliases.begin(), data, size - written);
    if (result >= 0) {
      if (!write || *nonblocking || written + result == size)
        return written + result;
      written += result;
      continue;
    }
    if (result != -EAGAIN || *nonblocking)
      return written ? static_cast<long>(written) : result;
    const int error = Park(shared_from_this(), write ? POLLOUT : POLLIN);
    if (error) return written ? static_cast<long>(written) : error;
  }
}

void File::MarkExternal() { external = true; }

int Park(const std::shared_ptr<File>& file, short events, uint64_t value) {
  const auto* task = this_coro.get();
  assert(task && !IsWaitingForIo(task));
  int error = 0;
  waiters.push_back({task, file, events, value, &error});
  do {
    CoroCtxGuard guard;
    CoroYield();
  } while (IsWaitingForIo(task));
  // A different task may close the last alias after readiness was observed.
  if (file->aliases.empty()) return -EBADF;
  return error;
}

}  // namespace io

bool IsWaitingForIo(const CoroBase* task) {
  SchedCtxGuard guard;
  const auto waiter =
      std::find_if(io::waiters.begin(), io::waiters.end(),
                   [task](const auto& waiter) { return waiter.task == task; });
  if (waiter == io::waiters.end()) return false;
  if (!io::CanContinue(*waiter)) return true;
  io::waiters.erase(waiter);
  return false;
}

bool HasPendingExternalIo() { return !io::ExternalSources().empty(); }

bool WaitForPendingIo(const CoroBase* task) {
  SchedCtxGuard guard;
  const bool ready = io::PollIoWaiters();
  if (task ? !IsWaitingForIo(task) : ready) return true;
  auto descriptors = io::ExternalSources(task);
  if (descriptors.empty()) return false;
  const long result = syscall_no_intercept(SYS_poll, descriptors.data(),
                                           descriptors.size(), -1);
  if (result < 0) {
    std::erase_if(io::waiters, [&](const auto& waiter) {
      if (task && waiter.task != task) return false;
      *waiter.error = static_cast<int>(result);
      return true;
    });
  } else {
    io::PollIoWaiters();
  }
  return true;
}

}  // namespace ltest
