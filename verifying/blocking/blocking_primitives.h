#pragma once
#include "runtime/include/lib.h"
struct safe_shared_mutex {
  safe_shared_mutex() {
    locked = 0;
    locked_addr = reinterpret_cast<long>(&locked);
  }
  void lock() {
    while (locked != 0) {
      this_coro->SetBlocked(locked_addr, locked);
      CoroYield();
    }
    locked = -1;
  }
  void unlock() { locked = 0; }
  void lock_shared() {
    while (locked == -1) {
      this_coro->SetBlocked(locked_addr, locked);
      CoroYield();
    }
    ++locked;
  }
  void unlock_shared() { --locked; }

 private:
  int locked;
  long locked_addr;
};