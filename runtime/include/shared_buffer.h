#pragma once

#include <sys/mman.h>
#include <unistd.h>

#include <algorithm>
#include <cstddef>
#include <stdexcept>
#include <utility>

namespace ltest {

struct Buffer {
  Buffer() {
    fd = memfd_create("ltest-attempt", MFD_CLOEXEC);
    if (fd < 0) throw std::runtime_error("memfd creation failed");
  }

  // Keep fixed strategy data mapped while the following region grows.
  explicit Buffer(size_t fixed_size) : Buffer() {
    Reserve(fixed_size);
    fixed = std::exchange(mapping, MAP_FAILED);
    const size_t page_size = getpagesize();
    offset = (fixed_size + page_size - 1) / page_size * page_size;
    capacity = 0;
  }

  Buffer(const Buffer&) = delete;
  Buffer& operator=(const Buffer&) = delete;

  ~Buffer() {
    if (mapping != MAP_FAILED) munmap(mapping, capacity);
    if (fixed != MAP_FAILED) munmap(fixed, offset);
    if (fd >= 0) close(fd);
  }

  void Map(size_t size) {
    void* next =
        mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
    if (next == MAP_FAILED) throw std::runtime_error("memfd mapping failed");
    if (mapping != MAP_FAILED) munmap(mapping, capacity);
    mapping = next;
    capacity = size;
  }

  void Reserve(size_t size) {
    if (size <= capacity) return;
    size = std::max(size, capacity * 2);
    if (ftruncate(fd, offset + size))
      throw std::runtime_error("memfd resize failed");
    Map(size);
  }

  int fd = -1;
  void* mapping = MAP_FAILED;
  size_t capacity = 0;
  void* fixed = MAP_FAILED;
  size_t offset = 0;
};

}  // namespace ltest
