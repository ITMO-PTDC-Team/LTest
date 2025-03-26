#include <linux/futex.h>

#include <atomic>
#include <cstdint>

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
  non_atomic int Lock() {
    if (CompareExchange(0, 1) == 0) {
      return 0;
    }
    while (CompareExchange(0, 2) == 1) {
      if (CompareExchange(1, 2) > 0) {
        while (locked_.load() == 2) {
          FutexWait(Addr(locked_), 2);
        }
      }
    }
    return 0;
  }

  non_atomic int Unlock() {
    if (locked_.fetch_sub(1) != 1) {
      locked_.store(0);
      FutexWake(Addr(locked_), 1);
    }
    return 0;
  }

  void Release() { locked_.store(0); }

  void Reset() { locked_.store(0); }

 private:
  std::atomic_int32_t locked_{0};
};

target_method(ltest::generators::genEmpty, int, Mutex, Lock);

target_method(ltest::generators::genEmpty, int, Mutex, Unlock);

using spec_t = ltest::Spec<Mutex, spec::LinearMutex, spec::LinearMutexHash,
                           spec::LinearMutexEquals>;

LTEST_ENTRYPOINT_CONSTRAINT(spec_t, MutexVerifier);
