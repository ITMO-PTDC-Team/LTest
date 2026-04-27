#pragma once

#include <cstddef>
#include <deque>

class MemoryHandler {
  // this memory is acuired using malloc/new
  std::deque<void*> memory;

 public:
  void* RememberPointer(void*);
  void ForgetAboutPointer(void*);
  void FreeAllMemory();
};

extern MemoryHandler* memory_handler;

extern "C" void* LtestMemAlloc(std::size_t size);

extern "C" void LtestMemDealloc(void* ptr);