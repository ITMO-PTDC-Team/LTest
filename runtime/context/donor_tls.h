#pragma once

#include <pthread.h>

#include <atomic>

#include "debugger.h"

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
  bool ResumeCleanup();
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
  DebugThread debug_;
};

}  // namespace ltest
