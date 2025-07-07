#include <cassert>
#include <functional>
#include <map>
#include <string>

#include "runtime/include/value_wrapper.h"

namespace spec {

struct LinearMutex;

using mutex_method_t = std::function<ValueWrapper(LinearMutex *l, void *)>;

struct LinearMutex {
 private:
  int isLocked = 0;

 public:
  int Lock() {
    if (isLocked) {
      return 1;
    }
    isLocked = 1;
    return 0;
  }
  int Unlock() {
    isLocked = 0;
    return 0;
  }

  static auto GetMethods() {
    mutex_method_t lock_func = [](LinearMutex *l, void *) -> int {
      return l->Lock();
    };

    mutex_method_t unlock_func = [](LinearMutex *l, void *) -> int {
      return l->Unlock();
    };

    return std::map<std::string, mutex_method_t>{
        {"Lock", lock_func},
        {"Unlock", unlock_func},
    };
  }
};

struct LinearMutexHash {
  size_t operator()(const LinearMutex &r) const { return 0; }
};

struct LinearMutexEquals {
  bool operator()(const LinearMutex &lhs, const LinearMutex &rhs) const {
    return false;
  }
};

struct SharedLinearMutex;

using shared_mutex_method_t =
    std::function<ValueWrapper(SharedLinearMutex *l, void *)>;

struct SharedLinearMutex {
 private:
  enum : int32_t { READER = 1, WRITER = -1, FREE = 0 };
  int locked_state = FREE;

 public:
  int lock() {
    if (locked_state != FREE) {
      return 1;
    }
    locked_state = WRITER;
    return 0;
  }
  int unlock() {
    locked_state = FREE;
    return 0;
  }

  int lock_shared() {
    if (locked_state == WRITER) {
      return 1;
    }
    locked_state += READER;
    return 0;
  }

  int unlock_shared() {
    locked_state -= READER;
    return 0;
  }

  static auto GetMethods() {
    shared_mutex_method_t lock_func = [](SharedLinearMutex *l, void *) -> int {
      return l->lock();
    };

    shared_mutex_method_t unlock_func =
        [](SharedLinearMutex *l, void *) -> int { return l->unlock(); };

    shared_mutex_method_t shared_lock_func =
        [](SharedLinearMutex *l, void *) -> int { return l->lock_shared(); };

    shared_mutex_method_t shared_unlock_func =
        [](SharedLinearMutex *l, void *) -> int { return l->unlock_shared(); };

    return std::map<std::string, shared_mutex_method_t>{
        {"lock", lock_func},
        {"unlock", unlock_func},
        {"lock_shared", shared_lock_func},
        {"unlock_shared", shared_unlock_func}};
  }
};

struct SharedLinearMutexHash {
  size_t operator()(const SharedLinearMutex &r) const { return 0; }
};

struct SharedLinearMutexEquals {
  bool operator()(const SharedLinearMutex &lhs,
                  const SharedLinearMutex &rhs) const {
    return false;
  }
};

}  // namespace spec
