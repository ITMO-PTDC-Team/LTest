#include "donor_tls.h"

#include <libsyscall_intercept_hook_point.h>
#include <linux/futex.h>
#include <sys/mman.h>
#include <sys/syscall.h>

#include <boost/context/protected_fixedsize_stack.hpp>
#include <boost/context/stack_traits.hpp>
#include <cassert>
#include <cerrno>
#include <limits>
#include <new>
#include <stdexcept>
#include <system_error>

#include "../include/coro_ctx_guard.h"
#include "../include/fiber_stack.h"
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
  donor.debug_.donor_tid = syscall_no_intercept(SYS_gettid);
  donor.SetState(Parked);
  donor.WaitWhile(Parked);
  zombie_ = &donor;
  ltest_coro_ctx = true;
  // libc runs the registered C++ and pthread-key destructors on this thread.
  return nullptr;
}

DonorTls::DonorTls(std::size_t logical_id) {
  SchedCtxGuard scheduler;
  debug_.logical_id = logical_id;
  if (!debug_executor_tid)
    debug_executor_tid = syscall_no_intercept(SYS_gettid);

  auto check = [](int error, const char* operation) {
    if (error)
      throw std::system_error(error, std::generic_category(), operation);
  };
  pthread_attr_t attributes;
  check(pthread_attr_init(&attributes), "initializing TLS donor attributes");
  boost::context::stack_context mapping;
  try {
    std::size_t native_size;
    check(pthread_attr_getstacksize(&attributes, &native_size),
          "reading TLS donor stack size");
    const auto page = boost::context::stack_traits::page_size();
    native_size = detail::FiberStackMappingSize(native_size) - page;
    stack_.size = detail::FiberStackMappingSize(GetFiberStackSize());
    const auto maximum = std::numeric_limits<std::size_t>::max();
    if (stack_.size > maximum - page ||
        native_size > maximum - stack_.size - page)
      throw std::bad_alloc();
    mapping = boost::context::protected_fixedsize_stack(stack_.size + native_size)
                  .allocate();
    stack_.sp = static_cast<char*>(mapping.sp) - native_size - page;
    // Separate downward-growing stacks: target execution below, native
    // parking and TLS cleanup above. pthread_attr_setstack supplies no guard.
    if (mprotect(stack_.sp, page, PROT_NONE))
      throw std::system_error(errno, std::generic_category(),
                              "guarding TLS donor stacks");
    check(pthread_attr_setstack(
              &attributes, static_cast<char*>(stack_.sp) + page, native_size),
          "setting TLS donor stack");
    check(pthread_create(&handle_, &attributes, Start, this),
          "creating TLS donor pthread");
  } catch (...) {
    pthread_attr_destroy(&attributes);
    if (mapping.sp)
      boost::context::protected_fixedsize_stack().deallocate(mapping);
    throw;
  }
  pthread_attr_destroy(&attributes);
  WaitWhile(Starting);
  debug_.next = debug_threads;
  debug_threads = &debug_;
}

boost::context::stack_context DonorTls::AcquireStack(std::size_t bytes) {
  if (stack_in_use_ || stack_.size != detail::FiberStackMappingSize(bytes))
    return {};
  stack_in_use_ = true;
  return stack_;
}

bool DonorTls::ReleaseStack(
    const boost::context::stack_context& stack) noexcept {
  if (stack.sp != stack_.sp) return false;
  assert(stack_in_use_ && stack.size == stack_.size);
  stack_in_use_ = false;
  return true;
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
      debug_.donor_tid = 0;
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
