#include <cstdio>

#include "runtime/include/scheduler.h"

struct MutexVerifier {
  bool Verify(const std::string& task_name, size_t thread_id) {
    debug(stderr, "validating method %s, thread_id: %zu\n", task_name.data(),
          thread_id);
    if (status.count(thread_id) == 0) {
      status[thread_id] = 0;
    }
    if (task_name == "Lock") {
      return status[thread_id] == 0;
    } else if (task_name == "Unlock") {
      return status[thread_id] == 1;
    } else {
      assert(false);
    }
  }

  void OnFinished(Task& task, size_t thread_id) {
    auto task_name = task->GetName();
    debug(stderr, "On finished method %s, thread_id: %zu\n", task_name.data(),
          thread_id);
    if (task_name == "Lock") {
      status[thread_id] = 1;
    } else if (task_name == "Unlock") {
      status[thread_id] = 0;
    }
  }

  std::optional<std::string> ReleaseTask(size_t thread_id) {
    if (status[thread_id] == 1) {
      return {"Unlock"};
    }
    return std::nullopt;
  }

  // NOTE(kmitkin): we cannot just store number of thread that holds mutex
  //                because Lock can finish before Unlock!
  std::unordered_map<size_t, size_t> status;
};
