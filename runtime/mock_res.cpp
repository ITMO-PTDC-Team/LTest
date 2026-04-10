#include "mock_res.h"

#include <sys/mman.h>

#include <algorithm>
#include <cassert>

#include "coro_ctx_guard.h"
#include "logger.h"

MemoryHandler* memory_handler;

void* MemoryHandler::Allocate(size_t size) {
  void* ptr = operator new(size);
  memory.push_back(ptr);
  return ptr;
}

void MemoryHandler::Deallocate(void* ptr) {
  auto it = std::find(memory.begin(), memory.end(), ptr);
  assert(it != memory.end());
  operator delete(*it);
  memory.erase(it);
}

void MemoryHandler::FreeAllMemory() {
  for (auto mem : memory) {
    operator delete(mem);
  }
  memory.clear();
  for (auto [mem, size] : raw_memory) {
    munmap(mem, size);
  }
  memory.clear();
}
void MemoryHandler::RememberRawPtr(void* ptr, std::size_t size) {
  debug("allocated %d \n", ptr);
  raw_memory.emplace_back(ptr, size);
}
void MemoryHandler::DeleteRawPtr(void* ptr, std::size_t size) {
  auto it =
      std::find(raw_memory.begin(), raw_memory.end(), std::pair{ptr, size});
  debug("deallocated %d \n", ptr);
  if (it != raw_memory.end()) {
    // its okay - guard arent perfect and some of allocations of boost fibers
    // are missed;
    return;
  }
  munmap(ptr, size);
  raw_memory.erase(it);
}

// we check to ltest_coro_ctx to support resmockpass instrumentation
extern "C" void* LtestMemAlloc(std::size_t size) {
  if (!ltest_coro_ctx) {
    return operator new(size);
  }
  return memory_handler->Allocate(size);
}

extern "C" void LtestMemDealloc(void* ptr) {
  if (!ltest_coro_ctx) {
    operator delete(ptr);
    return;
  }
  memory_handler->Deallocate(ptr);
}
