#include <linux/futex.h>

#include <atomic>
#include <cstdint>

#include "runtime/include/logger.h"
#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"
#include "verifiers/mutex_verifier.h"
#include "verifying/specs/mutex.h"

inline void FutexWait(int *value, int expected_value) {
  syscall(SYS_futex, value, FUTEX_WAIT_PRIVATE, expected_value, nullptr,
          nullptr, 0);
}

inline void FutexWake(int *value, int count) {
  syscall(SYS_futex, value, FUTEX_WAKE_PRIVATE, count, nullptr, nullptr, 0);
}

class Mutex {
 private:
  static int32_t *Addr(std::atomic_int32_t &atomic) {
    return reinterpret_cast<int32_t *>(&atomic);
  }
  int32_t CompareExchange(int32_t old, int32_t ne) {
    locked_.compare_exchange_strong(old, ne);
    return old;
  }

 public:
  NON_ATOMIC int Lock() {
    DEBUG(stderr, "Lock\n");
    if (CompareExchange(0, 1) == 0) {
      DEBUG(stderr, "Lock finished\n");
      return 0;
    }
    while (CompareExchange(0, 2) != 0) {
      if (CompareExchange(1, 2) > 0) {
        while (locked_.load() == 2) {
          FutexWait(Addr(locked_), 2);
        }
      }
    }
    DEBUG(stderr, "Lock finished with %d\n", locked_.load());
    return 0;
  }

  NON_ATOMIC int Unlock() {
    DEBUG(stderr, "Unlock\n");
    if (locked_.fetch_sub(1) != 1) {
      locked_.store(0);
      FutexWake(Addr(locked_), 1);
    }
    DEBUG(stderr, "Unlock finished\n");
    return 0;
  }

  void Reset() { locked_.store(0); }

 private:
  std::atomic_int32_t locked_{0};
};

using SpecT = ltest::Spec<Mutex, spec::LinearMutex, spec::LinearMutexHash,
                          spec::LinearMutexEquals>;

LTEST_ENTRYPOINT_CONSTRAINT(SpecT, MutexVerifier);

TARGET_METHOD(ltest::generators::GenEmpty, int, Mutex, Lock);

TARGET_METHOD(ltest::generators::GenEmpty, int, Mutex, Unlock);