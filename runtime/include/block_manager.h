#pragma once
#include <algorithm>
#include <cstdint>
#include <deque>
#include <unordered_map>

#include "block_state.h"

struct CoroBase;

struct BlockManager {
  // TODO(kmitkin): due to usage in as_atomic functions rewrite to custom hash
  // table & linked list
  std::unordered_map<std::uintptr_t, std::deque<CoroBase *>> queues;

  inline void BlockOn(BlockState state, CoroBase *coro) {
    if (!queues.contains(state.addr)) {
      queues[state.addr] = std::deque<CoroBase *>{};
    }
    queues[state.addr].push_back(coro);
  }

  inline bool IsBlocked(const BlockState &state, CoroBase *coro) {
    return state.addr &&
           std::find(queues[state.addr].begin(), queues[state.addr].end(),
                     coro) != queues[state.addr].end();
  }

  inline std::size_t UnblockOn(std::intptr_t addr, std::size_t max_wakes) {
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

  inline void UnblockAllOn(std::intptr_t addr) {
    if (!queues.contains(addr)) {
      return;
    }
    queues[addr].clear();
  }
};

inline BlockManager block_manager;
