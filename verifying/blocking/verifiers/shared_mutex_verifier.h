#include <cstdio>

#include "runtime/include/scheduler.h"

namespace spec {

struct SharedMutexVerifier {
  enum : int32_t { READER = 4, WRITER = 1, FREE = 0 };
  /// Verify checks the state of a mutex on starting of `ctask`
  bool Verify(const std::string& task_name, size_t thread_id) {
    debug(stderr, "validating method %s, thread_id: %zu\n", task_name.data(),
          thread_id);
    if (status.count(thread_id) == 0) {
      status[thread_id] = FREE;
    }
    /// When `lock` is executed, it is expected that current thread doesn't hold
    /// a mutex because otherwise we get recursive lock and UB
    if (task_name == "lock") {
      return status[thread_id] == FREE;
    } else if (task_name == "unlock") {
      return status[thread_id] == WRITER;
    } else if (task_name == "lock_shared") {
      return status[thread_id] == FREE;
    } else if (task_name == "unlock_shared") {
      return status[thread_id] == READER;
    } else {
      assert(false);
    }
  }

  void OnFinished(Task& task, size_t thread_id) {
    auto task_name = task->GetName();
    debug(stderr, "On finished method %s, thread_id: %zu\n", task_name.data(),
          thread_id);
    if (task_name == "lock") {
      status[thread_id] = WRITER;
    } else if (task_name == "unlock") {
      status[thread_id] = FREE;
    } else if (task_name == "lock_shared") {
      status[thread_id] = READER;
    } else if (task_name == "unlock_shared") {
      status[thread_id] = FREE;
    } else {
      assert(false);
    }
  }

  std::optional<std::string> ReleaseTask(size_t thread_id) {
    if (status[thread_id] == WRITER) {
      return {"unlock"};
    } else if (status[thread_id] == READER) {
      return {"unlock_shared"};
    }
    return std::nullopt;
  }

  std::unordered_map<size_t, size_t> status;
};
}  // namespace spec
