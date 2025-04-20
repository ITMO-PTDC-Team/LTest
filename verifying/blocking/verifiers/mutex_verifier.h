#include <cstdio>

#include "runtime/include/scheduler.h"

struct MutexVerifier {
  bool Verify(CreatedTaskMetaData ctask) {
    auto [taskName, is_new, thread_id] = ctask;
    debug(stderr, "validating method %s, thread_id: %zu\n", taskName.data(),
          thread_id);
    if (!is_new) {
      return true;
    }
    if (status.count(thread_id) == 0) {
      status[thread_id] = 0;
    }
    if (taskName == "Lock") {
      return status[thread_id] == 0;
    } else if (taskName == "Unlock") {
      return status[thread_id] == 1;
    } else {
      assert(false);
    }
  }

  void OnFinished(TaskWithMetaData ctask) {
    auto [task, is_new, thread_id] = ctask;
    auto taskName = task->GetName();
    debug(stderr, "On finished method %s, thread_id: %zu\n", taskName.data(),
          thread_id);
    if (taskName == "Lock") {
      status[thread_id] = 1;
    } else if (taskName == "Unlock") {
      status[thread_id] = 0;
    }
  }

  void Reset() { status.clear(); }

  void UpdateState(std::string_view, int, bool){}

  // NOTE(kmitkin): we cannot just store number of thread that holds mutex
  //                because Lock can finish before Unlock!
  std::unordered_map<size_t, size_t> status;
};
