#include "mock_res.h"

#include <algorithm>

#include "coro_ctx_guard.h"

MemoryHandler* memory_handler;

void* MemoryHandler::Allocate(size_t size) {
  void* ptr = operator new(size);
  memory.push_back(ptr);
  return ptr;
}

void MemoryHandler::Deallocate(void* ptr) {
  for (auto m : memory) {
  }
  auto it = std::find(memory.begin(), memory.end(), ptr);
  if (it == memory.end()) {
    return;
  }
  operator delete(*it);
  memory.erase(it);
}

void MemoryHandler::FreeAllMemory() {
  for (auto mem : memory) {
    operator delete(mem);
  }
  memory.clear();
}

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
