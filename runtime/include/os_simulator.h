#pragma once
#include <cstddef>

#include "mock_res.h"

class OSSimulator {
  MemoryHandler os_memory;

 public:
  OSSimulator() { memory_handler = &os_memory; }
  bool CanThreadContinue(std::size_t number);
  void UpdateState();
  void ResetState();
  ~OSSimulator() { ResetState(); }
};