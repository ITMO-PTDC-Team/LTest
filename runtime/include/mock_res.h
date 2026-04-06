#pragma once

#include <cstddef>
#include <deque>

class MemoryHandler {
  //this memory is acuired using malloc/new
  std::deque<void*> memory;
  //this is allocated using mmap
  std::deque<std::pair<void*, std::size_t>> raw_memory;

 public:
  void* Allocate(size_t);
  void Deallocate(void*);
  void FreeAllMemory();
  void RememberRawPtr(void* ptr, std::size_t);
  void DeleteRawPtr(void* ptr, std::size_t size);
};

extern MemoryHandler* memory_handler;

extern "C" void* LtestMemAlloc(std::size_t size);

extern "C" void LtestMemDealloc(void* ptr);