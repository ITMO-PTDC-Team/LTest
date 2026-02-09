#pragma once

#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

namespace ltest {

// Reserve rule (Blocked-trigger MVP).
struct ReserveRule {
  std::string wait_method;                    // e.g. "pop", "receive", "send"
  std::vector<std::string> progress_methods;  // e.g. {"push"}, {"send"}, {"receive"}
  std::size_t reserve_threads = 1;            // usually 1
};

// Workload policy specified by the sequential spec (optional).
struct WorkloadPolicy {
  std::vector<ReserveRule> reserve;
};

// Context passed to verifier when we are about to START a new task in a free thread.
struct StartContext {
  std::size_t threads = 0;

  // How many threads are currently free (can start a new task):
  // threads[i].empty() || threads[i].back()->IsReturned()
  std::size_t free_threads = 0;

  std::unordered_map<std::string, int> active_by_method;
  // Count of blocked active tasks by method name (Blocked-trigger).
  // Example: {"pop": 3, "receive": 1}
  std::unordered_map<std::string, int> blocked_by_method;
};

}  // namespace ltest