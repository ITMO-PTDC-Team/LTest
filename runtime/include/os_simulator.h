#pragma once
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <variant>

#include "lib.h"
#include "mock_res.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"
#include "stable_vector.h"

namespace ltest {

struct CreatedThreadInfo {
  std::function<void*(void*)> function;
  std::string name;
};

struct JoinThreadInfo {
  uint64_t thread_id;
  void** retval;
};

extern uint64_t current_max_thread_id;
extern std::optional<std::variant<CreatedThreadInfo, JoinThreadInfo>> thread_info;

void ResetStaticStorage();
void RunStaticConstructors();

inline std::string GetThreadResultToString(const ValueWrapper&) {
  return "Thread result";
}

class OSSimulator {
  MemoryHandler os_memory;
  std::unordered_map<uint64_t, std::pair<uint64_t, void**>> join_pairs;

 public:
  OSSimulator();
  virtual ~OSSimulator() {
    context::fiber_context::FreeForgottenStacks();
    memory_handler->FreeAllMemory();
    memory_handler = nullptr;
  }
  bool CanThreadContinue(std::size_t number);
  void UpdateOSState(size_t thread, Scheduler::SeqHistory& seq,
                     FullHistoryWithThreads& full);
  void ResetOSState();

 protected:
  std::vector<StableVector<Task>> threads;
};

}  // namespace ltest
