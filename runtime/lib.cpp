#include "include/lib.h"

#include <cassert>
#include <utility>
#include <vector>

#include "coro_ctx_guard.h"
#include "logger.h"
#include "value_wrapper.h"

// See comments in the lib.h.
Task this_coro{};

boost::context::fiber_context sched_ctx;
std::optional<CoroutineStatus> coroutine_status;

namespace ltest {
std::vector<TaskBuilder> task_builders{};
}

Task CoroBase::GetPtr() { return shared_from_this(); }

void CoroBase::Resume() {
  this_coro = this->GetPtr();
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

void CoroBase::Terminate() {
  int tries = 0;
  while (!IsReturned()) {
    ++tries;
    Resume();
    assert(tries < 1000000 &&
           "coroutine is spinning too long, possible wrong terminating order");
  }
}

void CoroBase::TryTerminate() {
  for (size_t i = 0; i < 1000 && !is_returned; ++i) {
    Resume();
  }
}