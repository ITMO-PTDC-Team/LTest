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

struct shared_mutex_r {
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

struct shared_mutex {
  as_atomic void lock() {
    std::unique_lock lock{mutex_};
    while (write_) {
      write_entered_.wait(lock);
    }
    write_ = true;
    while (reader_count_ > 0) {
      no_readers_.wait(lock);
    }
  }

  as_atomic void unlock() {
    std::unique_lock lock{mutex_};
    write_ = false;
    write_entered_.notify_all();
  }

  as_atomic void lock_shared() {
    std::unique_lock lock{mutex_};
    while (write_) {
      write_entered_.wait(lock);
    }
    ++reader_count_;
  }
  as_atomic void unlock_shared() {
    std::unique_lock lock{mutex_};
    --reader_count_;
    if (write_ && reader_count_ == 0) {
      no_readers_.notify_one();
    }
  }

 private:
  int reader_count_{0};
  bool write_{false};

  ltest::condition_variable write_entered_;
  ltest::condition_variable no_readers_;

  ltest::mutex mutex_;
};

}  // namespace ltest
