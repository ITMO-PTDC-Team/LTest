#include <libsyscall_intercept_hook_point.h>
#include <poll.h>
#include <sys/epoll.h>
#include <sys/syscall.h>

#include <algorithm>
#include <cerrno>
#include <map>

#include "io.h"

namespace ltest::io {
namespace {

struct Watch {
  std::shared_ptr<File> file;
  int registered_fd;
  epoll_event event;
  bool enabled = true;
};

struct Epoll final : File {
  Epoll() { external = false; }

  bool CanMakeExternalProgress() const override {
    if (aliases.empty()) return false;
    if (external) return true;
    // Linux rejects cycles and excessive nesting when registering epoll fds.
    return std::any_of(watches.begin(), watches.end(), [](const auto& item) {
      const auto& watch = item.second;
      return watch.enabled && watch.file->CanMakeExternalProgress();
    });
  }

  void Forget(const File* file) override {
    if (file == this)
      watches.clear();
    else
      std::erase_if(watches, [file](const auto& item) {
        return item.second.file.get() == file;
      });
  }

  std::map<uint64_t, Watch> watches;
  uint64_t next_id = 1;
};

}  // namespace

void RegisterEpoll(int fd) { RegisterFile(fd, std::make_shared<Epoll>()); }

long ControlEpoll(int epfd, int operation, int fd, const epoll_event* event) {
  auto file = std::dynamic_pointer_cast<Epoll>(FindFile(epfd));
  if (!file)
    return syscall_no_intercept(SYS_epoll_ctl, epfd, operation, fd, event);
  auto watched = TrackFile(fd);
  auto it = std::find_if(
      file->watches.begin(), file->watches.end(), [&](const auto& item) {
        return item.second.registered_fd == fd && item.second.file == watched;
      });
  const uint64_t id = it == file->watches.end() ? file->next_id : it->first;
  epoll_event native{};
  if (operation != EPOLL_CTL_DEL && event) {
    // Exclusive kernel wake selection does not describe logical waiters.
    if (event->events & EPOLLEXCLUSIVE) return -EOPNOTSUPP;
    native = *event;
    native.data.u64 = id;
  }
  const long result = syscall_no_intercept(SYS_epoll_ctl, epfd, operation, fd,
                                           event ? &native : nullptr);
  if (result < 0) return result;
  if (operation == EPOLL_CTL_DEL) {
    file->watches.erase(it);
  } else if (operation == EPOLL_CTL_ADD) {
    file->watches.emplace(id, Watch{watched, fd, *event});
    ++file->next_id;
  } else {
    it->second.event = *event;
    it->second.enabled = true;
  }
  return result;
}

long WaitEpoll(int fd, epoll_event* events, int count, bool nonblocking) {
  auto file = TrackFile(fd);
  auto epoll = std::dynamic_pointer_cast<Epoll>(file);
  for (;;) {
    // A parked call retains its identity through descriptor close/reuse.
    const int source =
        file && !file->aliases.empty() ? *file->aliases.begin() : fd;
    const long result =
        syscall_no_intercept(SYS_epoll_wait, source, events, count, 0);
    if (result > 0 && epoll) {
      for (long i = 0; i < result; ++i) {
        auto& watch = epoll->watches.at(events[i].data.u64);
        events[i].data = watch.event.data;
        if (watch.event.events & EPOLLONESHOT) watch.enabled = false;
      }
    }
    if (result != 0 || nonblocking) return result;
    const int error = Park(file, POLLIN);
    if (error) return error;
    // Readiness does not reserve an event for this particular waiter.
  }
}

}  // namespace ltest::io
