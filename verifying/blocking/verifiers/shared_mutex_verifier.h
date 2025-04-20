#include <cstdio>

#include "runtime/include/scheduler.h"

struct SharedMutexVerifier {
  enum : int32_t { READER = 4, WRITER = 1, FREE = 0 };
  /// Verify checks the state of a mutex on starting of `ctask`
  bool Verify(CreatedTaskMetaData ctask) {
    auto [taskName, is_new, thread_id] = ctask;
    debug(stderr, "validating method %s, thread_id: %zu\n", taskName.data(),
          thread_id);
    if (status.count(thread_id) == 0) {
      status[thread_id] = FREE;
    }
    /// When `lock` is executed, it is expected that current thread doesn't hold
    /// a mutex because otherwise we get recursive lock and UB
    if (taskName == "lock") {
      return status[thread_id] == FREE;
    } else if (taskName == "unlock") {
      return status[thread_id] == WRITER;
    } else if (taskName == "lock_shared") {
      return status[thread_id] == FREE;
    } else if (taskName == "unlock_shared") {
      return status[thread_id] == READER;
    } else {
      assert(false);
    }
  }

  void OnFinished(TaskWithMetaData ctask) {
    auto [task, is_new, thread_id] = ctask;
    auto taskName = task->GetName();
    debug(stderr, "On finished method %s, thread_id: %zu\n", taskName.data(),
          thread_id);
    if (taskName == "lock") {
      status[thread_id] = WRITER;
    } else if (taskName == "unlock") {
      status[thread_id] = FREE;
    } else if (taskName == "lock_shared") {
      status[thread_id] = READER;
    } else if (taskName == "unlock_shared") {
      status[thread_id] = FREE;
    } else {
      assert(false);
    }
  }

  void Reset() { status.clear(); }

  void UpdateState(std::string_view, int, bool) {}

  std::unordered_map<size_t, size_t> status;
};
