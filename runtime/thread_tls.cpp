#include "include/thread_tls.h"

#include <dlfcn.h>

#include <exception>
#include <memory>
#include <stdexcept>
#include <utility>
#include <vector>

#include "context/donor_tls.h"
#include "include/lib.h"

namespace ltest {

const unsigned debug_abi_version = 1;
long debug_executor_tid = 0;
DebugThread* debug_threads = nullptr;

void* CurrentTlsBase() noexcept {
  void* pointer;
  asm volatile("rdfsbase %0" : "=r"(pointer));
  return pointer;
}

namespace {

struct ThreadTlsState {
  void* native = CurrentTlsBase();
  std::vector<std::unique_ptr<DonorTls>> threads;
};

}  // namespace

DonorTls& GetLogicalThreadTls(std::size_t index) {
  // Each attempt runs in a fresh child. Keep records until _exit, including
  // those still referenced by parked donors when an exception unwinds.
  static auto* state = new ThreadTlsState();
  if (state->threads.size() <= index) state->threads.resize(index + 1);
  auto& donor = state->threads[index];
  if (!donor) {
    if (CurrentTlsBase() != state->native)
      throw std::runtime_error("TLS allocation requires scheduler TLS");
    donor = std::make_unique<DonorTls>(index);
  }
  return *donor;
}

void RunWithLogicalThreadTls(std::size_t thread,
                             const std::function<void()>& function) {
  auto& tls = GetLogicalThreadTls(thread);
  void* pointer = tls.ThreadPointer();
  std::exception_ptr failure;
  boost::context::fiber call(std::allocator_arg, MakeFiberStack(),
                             [&](boost::context::fiber&& scheduler) {
                               const int previous = this_thread_id;
                               this_thread_id = thread;
                               try {
                                 CoroCtxGuard guard;
                                 function();
                               } catch (...) {
                                 failure = std::current_exception();
                               }
                               this_thread_id = previous;
                               return std::move(scheduler);
                             });
  call.set_tls_base(pointer);
  {
    DebugFiberScope debug(tls.DebugInfo(), call, "target construction");
    call = std::move(call).resume();
  }
  if (failure) std::rethrow_exception(failure);
}

}  // namespace ltest
