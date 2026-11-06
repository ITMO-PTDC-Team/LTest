#pragma once

#include <boost/context/fiber.hpp>
#include <cstddef>
#include <exception>
#include <string_view>

namespace ltest {

// Read by scripts/ltest_gdb.py. Keep this record independent of STL containers
// and glibc's private pthread layout. Records live until the worker's _exit.
struct DebugThread {
  DebugThread* next = nullptr;
  std::size_t logical_id = 0;
  long donor_tid = 0;
  bool running = false;
  boost::context::fiber* fiber = nullptr;
  const char* task_name = nullptr;
  std::size_t task_name_size = 0;
};

extern const unsigned debug_abi_version;
extern long debug_executor_tid;
extern DebugThread* debug_threads;

class DebugFiberScope {
 public:
  DebugFiberScope(DebugThread& thread, boost::context::fiber& fiber,
                  std::string_view name)
      : thread_(thread),
        fiber_(fiber),
        exceptions_(std::uncaught_exceptions()) {
    thread_.running = true;
    thread_.fiber = &fiber_;
    thread_.task_name = name.data();
    thread_.task_name_size = name.size();
  }

  ~DebugFiberScope() {
    if (std::uncaught_exceptions() != exceptions_ || !fiber_) {
      thread_.fiber = nullptr;
      thread_.task_name = nullptr;
      thread_.task_name_size = 0;
    }
    thread_.running = false;
  }

  DebugFiberScope(const DebugFiberScope&) = delete;
  DebugFiberScope& operator=(const DebugFiberScope&) = delete;

 private:
  DebugThread& thread_;
  boost::context::fiber& fiber_;
  int exceptions_;
};

}  // namespace ltest
