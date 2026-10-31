#include "donor_tls.h"

#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <sys/syscall.h>

#include <cassert>
#include <cerrno>
#include <stdexcept>
#include <system_error>

#include "../include/coro_ctx_guard.h"
#include "../include/thread_tls.h"

namespace ltest {

void DonorTls::SetState(State next) {
  state_.store(next, std::memory_order_release);
  syscall_no_intercept(SYS_futex, &state_, FUTEX_WAKE_PRIVATE, 1, nullptr,
                       nullptr, 0);
}

// No libc or TLS access: the executor may be borrowing the donor's TLS.
void DonorTls::WaitWhile(State expected, const timespec* timeout) {
  while (state_.load(std::memory_order_acquire) == expected) {
    syscall_no_intercept(SYS_futex, &state_, FUTEX_WAIT_PRIVATE, expected,
                         timeout, nullptr, 0);
    // Let the caller poll native exit between timed waits.
    if (timeout) return;
  }
}

thread_local DonorTls* DonorTls::zombie_ = nullptr;

void* DonorTls::Start(void* argument) {
  auto& donor = *static_cast<DonorTls*>(argument);
  donor.pointer_ = CurrentTlsBase();
  donor.SetState(Parked);
  donor.WaitWhile(Parked);
  zombie_ = &donor;
  ltest_coro_ctx = true;
  // libc runs the registered C++ and pthread-key destructors on this thread.
  return nullptr;
}

DonorTls::DonorTls() {
  SchedCtxGuard scheduler;
  int error = pthread_create(&handle_, nullptr, Start, this);
  if (error)
    throw std::system_error(error, std::generic_category(),
                            "creating TLS donor pthread");
  WaitWhile(Starting);
}

void* DonorTls::ThreadPointer() const {
  if (!pointer_) throw std::runtime_error("cannot resume an exited TLS thread");
  return pointer_;
}

void DonorTls::RequestCleanup() {
  assert(CurrentTlsBase() == pointer_);
  // Request only; the donor stays parked until the executor gives back TLS.
  cleanup_requested_ = true;
}

bool DonorTls::ResumeCleanup() {
  if (!cleanup_requested_) return true;
  assert(pointer_ && CurrentTlsBase() != pointer_);
  SetState(Running);
  while (state_.load(std::memory_order_acquire) == Running) {
    const int error = pthread_tryjoin_np(handle_, nullptr);
    if (!error) {
      pointer_ = nullptr;
      return true;
    }
    if (error != EBUSY)
      throw std::system_error(error, std::generic_category(),
                              "joining TLS donor pthread");
    // Native exit has no final CoroYield. Poll completion using the public
    // pthread API, without depending on glibc's private TCB layout.
    const timespec retry{0, 100000};
    WaitWhile(Running, &retry);
  }
  return false;
}

bool DonorTls::YieldCleanup() {
  if (!zombie_) return false;
  SchedCtxGuard guard;
  zombie_->SetState(Yielded);
  zombie_->WaitWhile(Yielded);
  return true;
}

}  // namespace ltest
