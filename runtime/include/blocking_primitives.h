#pragma once
#include <mutex>

#include "block_manager.h"
#include "lib.h"
#include "verifying_macro.h"

namespace ltest {

struct mutex {
  as_atomic void lock() {
    while (locked) {
      this_coro->SetBlocked(state);
      CoroYield();
    }
    locked = 1;
  }

  as_atomic bool try_lock() {
    if (locked) {
      CoroYield();
      return false;
    }
    locked = 1;
    return true;
  }

  as_atomic void unlock() {
    locked = 0;
    block_manager.UnblockAllOn(
        state.addr);  // To have the ability schedule any coroutine
  }

 private:
  int locked{0};
  BlockState state{reinterpret_cast<std::intptr_t>(&locked), locked};

  friend struct condition_variable;
};

struct shared_mutex {
  as_atomic void lock() {
    while (locked != 0) {
      this_coro->SetBlocked(state);
      CoroYield();
    }
    locked = -1;
  }
  as_atomic void unlock() {
    locked = 0;
    block_manager.UnblockAllOn(state.addr);
  }
  as_atomic void lock_shared() {
    while (locked == -1) {
      this_coro->SetBlocked(state);
      CoroYield();
    }
    ++locked;
  }
  as_atomic void unlock_shared() {
    --locked;
    block_manager.UnblockAllOn(state.addr);
  }

 private:
  int locked{0};
  BlockState state{reinterpret_cast<std::intptr_t>(&locked), locked};
};

struct condition_variable {
  as_atomic void wait(std::unique_lock<ltest::mutex>& lock) {
    addr = lock.mutex()->state.addr;
    lock.unlock();
    this_coro->SetBlocked({addr, 1});
    CoroYield();
    lock.lock();
  }

  as_atomic void notify_one() { block_manager.UnblockOn(addr, 1); }

  as_atomic void notify_all() { block_manager.UnblockAllOn(addr); }

 private:
  std::intptr_t addr;
};

}  // namespace ltest
