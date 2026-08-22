#pragma once
#include <iostream>
#include <memory_resource>
#include <mutex>
#include <variant>

#include "block_manager.h"
#include "lib.h"
#include "verifying_macro.h"

namespace ltest {

struct mutex {
  as_atomic void lock() {
    while (locked) {
      CoroCtxGuard guard;
      this_coro->SetBlocked(state());
      CoroYield();
    }
    locked = 1;
  }

  as_atomic bool try_lock() {
    if (locked) {
      CoroCtxGuard guard;
      CoroYield();
      return false;
    }
    locked = 1;
    return true;
  }

  as_atomic void unlock() {
    locked = 0;
    // To have the ability schedule any coroutine.
    block_manager.UnblockAllOn(addr());
  }

 private:
  [[nodiscard]] std::intptr_t addr() const {
    return reinterpret_cast<std::intptr_t>(&locked);
  }

  [[nodiscard]] BlockState state() const { return {addr(), locked}; }

  int locked{0};

  friend struct condition_variable;
};

struct recursive_mutex {
  as_atomic void lock() {
    if (owner_ == this_thread_id) {
      ++depth_;
      return;
    }
    while (owner_ != -1) {
      CoroCtxGuard guard;
      this_coro->SetBlocked(state());
      CoroYield();
    }
    owner_ = this_thread_id;
    depth_ = 1;
  }

  as_atomic bool try_lock() {
    if (owner_ == this_thread_id) {
      ++depth_;
      return true;
    }
    if (owner_ != -1) {
      CoroCtxGuard guard;
      CoroYield();
      return false;
    }
    owner_ = this_thread_id;
    depth_ = 1;
    return true;
  }

  as_atomic void unlock() {
    assert(owner_ == this_thread_id);
    if (--depth_ == 0) {
      owner_ = -1;
      block_manager.UnblockAllOn(addr());
    }
  }

 private:
  [[nodiscard]] std::intptr_t addr() const {
    return reinterpret_cast<std::intptr_t>(&owner_);
  }

  [[nodiscard]] BlockState state() const { return {addr(), owner_}; }

  int owner_{-1};
  unsigned int depth_{0};
};

struct condition_variable {
  as_atomic void wait(std::unique_lock<ltest::mutex>& lock) {
    lock.unlock();
    this_coro->SetBlocked({addr(), 1});
    {
      CoroCtxGuard guard;
      CoroYield();
    }
    lock.lock();
  }

  // Is needed to match pthread API, which does not have std::unique_lock.
  template <typename Mutex>
  as_atomic void wait(Mutex& lock) {
    lock.unlock();
    this_coro->SetBlocked({addr(), 1});
    {
      CoroCtxGuard guard;
      CoroYield();
    }
    lock.lock();
  }

  as_atomic void notify_one() { block_manager.UnblockOn(addr(), 1); }

  as_atomic void notify_all() { block_manager.UnblockAllOn(addr()); }

 private:
  [[nodiscard]] std::intptr_t addr() const {
    return reinterpret_cast<std::intptr_t>(&wait_queue_);
  }

  int wait_queue_{0};
};

/**
 *
 * shared_mutex is an advanced implementation with queues:
 *
 * Uses two condition_variables:
 * write_entered_ - to wait for the writer to log in
 * no_readers_ - to wait for readers to finish
 *
 * Be honest with the writers (FIFO)
 */
struct shared_mutex {
  as_atomic void lock() {
    std::unique_lock lock{mutex_};
    while (has_write()) {
      write_entered_.wait(lock);
    }
    write_ = this_thread_id;
    while (reader_count_ > 0) {
      no_readers_.wait(lock);
    }
  }
  as_atomic bool try_lock() {
    std::unique_lock lock{mutex_, std::try_to_lock};
    if (!lock.owns_lock() || has_write() || reader_count_ > 0) {
      return false;
    }
    write_ = this_thread_id;
    return true;
  }
  // in pthread api, unlock_shared calls under the hood unlock()
  as_atomic void unlock() {
    std::unique_lock lock{mutex_};
    if (write_ == this_thread_id) {
      write_ = -1;
      write_entered_.notify_all();
    }
    else {
      --reader_count_;
      if (has_write() && reader_count_ == 0) {
        no_readers_.notify_one();
      }
    }
  }

  as_atomic void lock_shared() {
    std::unique_lock lock{mutex_};
    while (has_write()) {
      write_entered_.wait(lock);
    }
    ++reader_count_;
  }
  as_atomic bool try_lock_shared() {
    std::unique_lock lock{mutex_, std::try_to_lock};
    if (!lock.owns_lock() || has_write()) {
      return false;
    }
    ++reader_count_;
    return true;
  }
  as_atomic void unlock_shared() {
    unlock();
  }

 private:
  [[nodiscard]] bool has_write() const { return write_ != -1ull; }
  int reader_count_{0};
  size_t write_ = -1;

  condition_variable write_entered_;
  condition_variable no_readers_;

  mutex mutex_;
};

inline std::pmr::monotonic_buffer_resource pthread_mock_resource(1000);
inline std::pmr::unordered_map<pthread_mutex_t *,
                               std::variant<mutex, recursive_mutex>>
    mutexes(&pthread_mock_resource);
inline std::pmr::unordered_map<pthread_rwlock_t *,
                               std::variant<shared_mutex>>
    shared_mutexes(&pthread_mock_resource);
inline std::pmr::unordered_map<pthread_cond_t *, condition_variable>
    cond_variables(&pthread_mock_resource);

}  // namespace ltest
