#pragma once
#include "lib.h"
#include "verifying_macro.h"

namespace ltest {

struct mutex {
  atomic void lock() {
    while (locked) {
      this_coro->SetBlocked(locked_addr, locked);
      CoroYield();
    }
    locked = 1;
  }

  atomic bool try_lock() {
    if (locked) {
      CoroYield();
      return false;
    }
    locked = 1;
    return true;
  }

  atomic void unlock() { 
    locked = 0; 
  }

 private:
  int locked{0};
  long locked_addr{reinterpret_cast<long>(&locked)};
};

struct shared_mutex {
  atomic void lock() {
    while (locked != 0) {
      this_coro->SetBlocked(locked_addr, locked);
      CoroYield();
    }
    locked = -1;
  }
  atomic void unlock() { locked = 0; }
  atomic void lock_shared() {
    while (locked == -1) {
      this_coro->SetBlocked(locked_addr, locked);
      CoroYield();
    }
    ++locked;
  }
  atomic void unlock_shared() { --locked; }

 private:
  int locked{0};
  long locked_addr{reinterpret_cast<long>(&locked)};
};
}  // namespace ltest
