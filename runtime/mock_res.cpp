#include "mock_res.h"

#include <sys/mman.h>

#include <algorithm>
#include <cassert>
#include <utility>

#include "coro_ctx_guard.h"
#include "logger.h"

namespace ltest {

MemoryHandler* memory_handler;

void* MemoryHandler::RememberPointer(void* ptr) {
  memory.emplace_back(ptr);
  return ptr;
}

void MemoryHandler::ForgetAboutPointer(void* ptr) {
  auto it = std::find(memory.begin(), memory.end(), ptr);
  if (it == memory.end()) {
    return;
  }
  memory.erase(it);
}

void MemoryHandler::FreeAllMemory() {
  std::deque<void*> to_free;
  to_free.swap(memory);

  for (auto mem : to_free) {
    free(mem);
  }
}

// we check to ltest_coro_ctx to support resmockpass instrumentation
extern "C" void* LtestMemAlloc(std::size_t size) {
  void* ptr = malloc(size);
  if (!ltest_coro_ctx) {
    return ptr;
  }
  return memory_handler->RememberPointer(ptr);
}

extern "C" void LtestMemDealloc(void* ptr) {
  free(ptr);
  if (!ltest_coro_ctx) {
    return;
  }
  memory_handler->ForgetAboutPointer(ptr);
}
}  // namespace ltest
