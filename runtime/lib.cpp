#include "include/lib.h"

#include <cassert>
#include <string>
#include <utility>
#include <vector>

#include "coro_ctx_guard.h"
#include "logger.h"
#include "value_wrapper.h"

// See comments in the lib.h.
Task this_coro{};
int this_thread_id = -1;

boost::context::fiber_context sched_ctx;
std::optional<CoroutineStatus> coroutine_status;

namespace ltest {
std::vector<TaskBuilder> task_builders{};
}

// Test failure tracking for litmus tests, which could expectedly fail.
namespace ltest {
namespace {
bool test_failed{false};
std::string test_failure_message{};
}  // namespace

void SetTestFailure(std::string message) {
  test_failure_message = std::move(message);
  test_failed = true;
}

bool HasTestFailure() { return test_failed; }

const std::string& GetTestFailureMessage() { return test_failure_message; }

void ClearTestFailure() {
  test_failed = false;
  test_failure_message.clear();
}
}  // namespace ltest

Task CoroBase::GetPtr() { return shared_from_this(); }

void CoroBase::Resume(int resumed_thread_id) {
  this_coro = this->GetPtr();
  this_thread_id = resumed_thread_id;
  assert(!this_coro->IsReturned() && this_coro->ctx);
  // debug(stderr, "name: %s\n",
  // std::string(this_coro->GetPtr()->GetName()).c_str());
  auto coro = this_coro.get();  // std::shared_ptr also can be interleaved
  // NOTE(kmitkin): Guard below prevents us from call CoroYield in the scheduler
  // coroutine, area that protected by it should be as small as possible to
  // reduce errors
  {
    ltest::CoroCtxGuard guard{};
    boost::context::fiber_context([coro](boost::context::fiber_context&& ctx) {
      sched_ctx = std::move(ctx);
      coro->ctx = std::move(coro->ctx).resume();
      return std::move(sched_ctx);
    }).resume();
  }
  this_coro.reset();
  this_thread_id = -1;
}

int CoroBase::GetId() const { return id; }

ValueWrapper CoroBase::GetRetVal() const {
  assert(IsReturned());
  return ret;
}

CoroBase::~CoroBase() {
  // The coroutine must be returned if we want to restart it.
  // We can't just Terminate() it because it is the runtime responsibility to
  // decide, in which order the tasks should be terminated.
  assert(IsReturned());
}

std::string_view CoroBase::GetName() const { return name; }

bool CoroBase::IsReturned() const { return is_returned; }

extern "C" void CoroYield() {
  if (!ltest_coro_ctx) [[unlikely]] {
    return;
  }
  assert(this_coro && sched_ctx);
  boost::context::fiber_context([](boost::context::fiber_context&& ctx) {
    this_coro->ctx = std::move(ctx);
    return std::move(sched_ctx);
  }).resume();
}

extern "C" void CoroutineStatusChange(char* name, bool start) {
  // assert(!coroutine_status.has_value());
  coroutine_status.emplace(name, start);
  CoroYield();
}

void CoroBase::Terminate(int running_thread_id) {
  int tries = 0;
  while (!IsReturned()) {
    ++tries;
    Resume(running_thread_id);
    assert(tries < 1000000 &&
           "coroutine is spinning too long, possible wrong terminating order");
  }
}

void CoroBase::TryTerminate(int running_thread_id) {
  for (size_t i = 0; i < 1000 && !is_returned; ++i) {
    Resume(running_thread_id);
  }
}