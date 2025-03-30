#pragma once
#include "futex.h"
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
    futex_queues.PopAll(
        state.addr);  // Two have the ability schedule any coroutine
  }

 private:
  int locked{0};
  FutexState state{reinterpret_cast<std::intptr_t>(&locked), locked};
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
    futex_queues.PopAll(state.addr);
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
    futex_queues.PopAll(state.addr);
  }

 private:
  int locked{0};
  FutexState state{reinterpret_cast<std::intptr_t>(&locked), locked};
};
}  // namespace ltest
