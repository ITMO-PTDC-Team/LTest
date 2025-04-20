#include <folly/Likely.h>
#include <folly/synchronization/Lock.h>

#include <atomic>
#include <thread>

#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"
#include "verifying/blocking/verifiers/shared_mutex_verifier.h"
#include "verifying/specs/mutex.h"

namespace folly {

class RWSpinLock {
  enum : int32_t { READER = 4, UPGRADED = 2, WRITER = 1 };

 public:
  RWSpinLock() = default;
  RWSpinLock(RWSpinLock const&) = delete;
  RWSpinLock& operator=(RWSpinLock const&) = delete;

  // Lockable Concept
  NON_ATOMIC int Lock() {
    uint_fast32_t count = 0;
    while (!LIKELY(TryLock())) {
      if (++count > 1000) {
        std::this_thread::yield();
      }
    }
    return 0;
  }

  // Writer is responsible for clearing up both the UPGRADED and WRITER bits.
  NON_ATOMIC int Unlock() {
    static_assert(READER > WRITER + UPGRADED, "wrong bits!");
    bits_.fetch_and(~(WRITER | UPGRADED), std::memory_order_release);
    return 0;
  }

  // SharedLockable Concept
  NON_ATOMIC int LockShared() {
    uint_fast32_t count = 0;
    while (!LIKELY(TryLockShared())) {
      if (++count > 1000) {
        std::this_thread::yield();
      }
    }
    return 0;
  }

  NON_ATOMIC int UnlockShared() {
    bits_.fetch_add(-READER, std::memory_order_release);
    return 0;
  }

  // Downgrade the lock from writer status to reader status.
  void UnlockAndLockShared() {
    bits_.fetch_add(READER, std::memory_order_acquire);
    Unlock();
  }

  // UpgradeLockable Concept
  void LockUpgrade() {
    uint_fast32_t count = 0;
    while (!TryLockUpgrade()) {
      if (++count > 1000) {
        std::this_thread::yield();
      }
    }
  }

  void UnlockUpgrade() {
    bits_.fetch_add(-UPGRADED, std::memory_order_acq_rel);
  }

  // unlock upgrade and try to acquire write lock
  void UnlockUpgradeAndLock() {
    int64_t count = 0;
    while (!TryUnlockUpgradeAndLock()) {
      if (++count > 1000) {
        std::this_thread::yield();
      }
    }
  }

  // unlock upgrade and read lock atomically
  void UnlockUpgradeAndLockShared() {
    bits_.fetch_add(READER - UPGRADED, std::memory_order_acq_rel);
  }

  // write unlock and upgrade lock atomically
  void UnlockAndLockUpgrade() {
    // need to do it in two steps here -- as the UPGRADED bit might be OR-ed at
    // the same time when other threads are trying do try_lock_upgrade().
    bits_.fetch_or(UPGRADED, std::memory_order_acquire);
    bits_.fetch_add(-WRITER, std::memory_order_release);
  }

  // Attempt to acquire writer permission. Return false if we didn't get it.
  bool TryLock() {
    int32_t expect = 0;
    return bits_.compare_exchange_strong(expect, WRITER,
                                         std::memory_order_acq_rel);
  }

  // Try to get reader permission on the lock. This can fail if we
  // find out someone is a writer or upgrader.
  // Setting the UPGRADED bit would allow a writer-to-be to indicate
  // its intention to write and block any new readers while waiting
  // for existing readers to finish and release their read locks. This
  // helps avoid starving writers (promoted from upgraders).
  bool TryLockShared() {
    // fetch_add is considerably (100%) faster than compare_exchange,
    // so here we are optimizing for the common (lock success) case.
    int32_t value = bits_.fetch_add(READER, std::memory_order_acquire);
    if (FOLLY_UNLIKELY(value & (WRITER | UPGRADED))) {
      bits_.fetch_add(-READER, std::memory_order_release);
      return false;
    }
    return true;
  }

  // try to unlock upgrade and write lock atomically
  bool TryUnlockUpgradeAndLock() {
    int32_t expect = UPGRADED;
    return bits_.compare_exchange_strong(expect, WRITER,
                                         std::memory_order_acq_rel);
  }

  // try to acquire an upgradable lock.
  bool TryLockUpgrade() {
    int32_t value = bits_.fetch_or(UPGRADED, std::memory_order_acquire);

    // Note: when failed, we cannot flip the UPGRADED bit back,
    // as in this case there is either another upgrade lock or a write lock.
    // If it's a write lock, the bit will get cleared up when that lock's done
    // with unlock().
    return ((value & (UPGRADED | WRITER)) == 0);
  }

  // mainly for debugging purposes.
  int32_t Bits() const { return bits_.load(std::memory_order_acquire); }

  void Reset() { bits_.store(0); }

 private:
  std::atomic<int32_t> bits_{0};
};

}  // namespace folly
using SpecT =
ltest::Spec<folly::RWSpinLock, spec::SharedLinearMutex,
spec::SharedLinearMutexHash, spec::SharedLinearMutexEquals>;

LTEST_ENTRYPOINT_CONSTRAINT(SpecT, SharedMutexVerifier);

TARGET_METHOD(ltest::generators::GenEmpty, int, folly::RWSpinLock, lock);
TARGET_METHOD(ltest::generators::GenEmpty, int, folly::RWSpinLock, LockShared);

TARGET_METHOD(ltest::generators::GenEmpty, int, folly::RWSpinLock, Unlock);
TARGET_METHOD(ltest::generators::GenEmpty, int, folly::RWSpinLock, unlock_shared);