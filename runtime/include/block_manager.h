#pragma once
#include <algorithm>
#include <cstdint>
#include <deque>
#include <unordered_map>

#include "block_state.h"
#include "coro_ctx_guard.h"

namespace ltest {

struct CoroBase;

struct BlockManager {
  // Maps a memory address to coroutines waiting on it.
  // TODO(kmitkin): due to usage in as_atomic functions rewrite to custom hash
  // table & linked list
  std::unordered_map<std::uintptr_t, std::deque<CoroBase *>> queues;

  void BlockOn(BlockState state, CoroBase *coro) {
    SchedCtxGuard guard;
    if (!queues.contains(state.addr)) {
      queues[state.addr] = std::deque<CoroBase *>{};
    }
    queues[state.addr].push_back(coro);
  }

  bool IsBlocked(const BlockState &state, CoroBase *coro) {
    SchedCtxGuard guard;
    return state.addr &&
           std::find(queues[state.addr].begin(), queues[state.addr].end(),
                     coro) != queues[state.addr].end();
  }

  std::size_t UnblockOn(std::intptr_t addr, std::size_t max_wakes) {
    SchedCtxGuard guard;
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

  void UnblockAllOn(std::intptr_t addr) {
    SchedCtxGuard guard;
    auto queue_it = queues.find(addr);
    if (queue_it == queues.end()) {
      return;
    }
    queue_it->second.clear();
  }

  void UnblockAll() { queues.clear(); }
};

inline BlockManager block_manager;

}  // namespace ltest
