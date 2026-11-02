#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <set>

struct epoll_event;

namespace ltest::io {

struct File : std::enable_shared_from_this<File> {
  virtual ~File() = default;

  std::set<int> aliases;
  // Present when the kernel fd is forced nonblocking for cooperative I/O.
  std::optional<bool> nonblocking;
  bool external = true;

  // True also covers unknown sources; false requires exclusively logical I/O.
  virtual bool CanMakeExternalProgress() const;
  virtual bool Ready(short events, uint64_t value) const;
  virtual long ReadWrite(bool write, void* buffer, size_t size);
  virtual void MarkExternal();
  virtual void Forget(const File*) {}
};

std::shared_ptr<File> FindFile(int fd);
std::shared_ptr<File> TrackFile(int fd);
void RegisterFile(int fd, const std::shared_ptr<File>& file);
void UnregisterFile(int fd);
void RegisterEventFd(int fd, unsigned value, int flags);
void RegisterPipe(int read_fd, int write_fd, int flags);
void RegisterEpoll(int fd);
void MarkFilesExternal();
int Park(const std::shared_ptr<File>& file, short events, uint64_t value = 0);

// Operations return raw Linux syscall results (negative errno).
long ControlEpoll(int epfd, int operation, int fd, const epoll_event* event);
// Nonblocking waits poll; other waits have no timeout expiration.
long WaitEpoll(int fd, epoll_event* events, int count, bool nonblocking);

}  // namespace ltest::io
