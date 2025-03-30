#pragma once
#include <algorithm>
#include <cstdint>
#include <deque>
#include <unordered_map>

#include "futex_state.h"

struct CoroBase;

struct FutexQueues {
  std::unordered_map<std::uintptr_t, std::deque<CoroBase *>> queues;

  void Push(FutexState state, CoroBase *coro) {
    if (!queues.contains(state.addr)) {
      queues[state.addr] = std::deque<CoroBase *>{};
    }
    queues[state.addr].push_back(coro);
  }

  bool IsBlocked(const FutexState &state, CoroBase *coro) {
    return state.addr &&
           std::find(queues[state.addr].begin(), queues[state.addr].end(),
                     coro) != queues[state.addr].end();
  }

  std::size_t Pop(std::intptr_t addr, std::size_t max_wakes) {
    if (!queues.contains(addr)) [[unlikely]] {
      return 0;
    }
    auto &queue = queues[addr];
    size_t wakes = 0;
    for (; wakes < max_wakes && !queue.empty(); ++wakes) {
      queue.pop_front();  // Can be spurious wake ups
    }
    return wakes;
  }

  void PopAll(std::intptr_t addr) { queues[addr].clear(); }
};

extern FutexQueues futex_queues;
