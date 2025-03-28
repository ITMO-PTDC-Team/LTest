#pragma once
#include "lib.h"


namespace ltest {

  struct mutex {

    void lock() {
      while (locked) {
        this_coro->SetBlocked(locked_addr, locked);
        CoroYield();
      }
      locked = 1;
    }

    bool try_lock() {
      if (locked) {
        CoroYield();
        return false;
      }
      locked = true;
      return true;
    }

    void unlock() {
      locked = 0;
    }

    private:
      int locked{0};
      long locked_addr{reinterpret_cast<long>(&locked)};
  };

  struct shared_mutex {
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
      int locked{0};
      long locked_addr{reinterpret_cast<long>(&locked)};
  };
}
