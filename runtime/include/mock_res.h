#pragma once

#include <cstddef>
#include <deque>

class MemoryHandler {
  std::deque<void*> memory;

 public:
  void* Allocate(size_t);
  void Deallocate(void*);
  void FreeAllMemory();
};

extern MemoryHandler* memory_handler;

extern "C" void* LtestMemAlloc(std::size_t size);

extern "C" void LtestMemDealloc(void* ptr);