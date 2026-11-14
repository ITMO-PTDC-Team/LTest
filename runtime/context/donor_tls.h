#pragma once

#include <pthread.h>

#include <atomic>
#include <boost/context/stack_context.hpp>

#include "../include/debugger.h"

namespace ltest {

// Resume native cleanup only after switching away from this TLS. A cleanup
// yield hands execution back to the scheduler until the next logical resume.
class DonorTls {
 public:
  explicit DonorTls(std::size_t logical_id);
  DonorTls(const DonorTls&) = delete;
  DonorTls& operator=(const DonorTls&) = delete;
  [[nodiscard]] void* ThreadPointer() const;
  DebugThread& DebugInfo() { return debug_; }
  void RequestCleanup();
  bool CleanupRequested() const { return cleanup_requested_; }
  bool ResumeCleanup();
  boost::context::stack_context AcquireStack(std::size_t bytes);
  bool ReleaseStack(const boost::context::stack_context& stack) noexcept;
  static bool YieldCleanup();
  static bool IsCleaningUp() { return zombie_ != nullptr; }

 private:
  enum State { Starting, Parked, Running, Yielded };
  static void* Start(void* argument);
  static thread_local DonorTls* zombie_;
  void SetState(State next);
  void WaitWhile(State expected, const timespec* timeout = nullptr);

  std::atomic<int> state_{Starting};
  pthread_t handle_{};
  void* pointer_ = nullptr;
  bool cleanup_requested_ = false;
  boost::context::stack_context stack_{};
  bool stack_in_use_ = false;
  DebugThread debug_;
};

}  // namespace ltest
