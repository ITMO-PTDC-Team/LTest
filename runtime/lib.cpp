#include "include/lib.h"

#include <cassert>
#include <utility>
#include <vector>

#include "logger.h"
#include "yield_guard.h"

// See comments in the lib.h.
Task this_coro{};

boost::context::fiber_context sched_ctx;

namespace ltest {
std::vector<TaskBuilder> task_builders{};
}

Task CoroBase::GetPtr() { return shared_from_this(); }

void CoroBase::SetToken(std::shared_ptr<Token> token) { this->token = token; }

void CoroBase::Resume() {
  this_coro = this->GetPtr();
  assert(!this_coro->IsReturned() && this_coro->ctx);
  ltest::YieldGuard guard{};
  debug(stderr, "name: %s\n",
        std::string(this_coro->GetPtr()->GetName()).c_str());
  boost::context::fiber_context([](boost::context::fiber_context&& ctx) {
    sched_ctx = std::move(ctx);
    this_coro->ctx = std::move(this_coro->ctx).resume();
    return std::move(sched_ctx);
  }).resume();
  this_coro.reset();
}

int CoroBase::GetRetVal() const {
  assert(IsReturned());
  return ret;
}

bool CoroBase::IsParked() const { return token != nullptr && token->parked; }

CoroBase::~CoroBase() {
  // The coroutine must be returned if we want to restart it.
  // We can't just Terminate() it because it is the runtime responsibility to
  // decide, in which order the tasks should be terminated.
  assert(IsReturned());
}

std::string_view CoroBase::GetName() const { return name; }

bool CoroBase::IsReturned() const { return is_returned; }

extern "C" void CoroYield() {
  if (!__yield) {
    return;
  }
  assert(this_coro && sched_ctx);
  debug(stderr, "Switch to %s\n", this_coro->GetName().data());
  boost::context::fiber_context([](boost::context::fiber_context&& ctx) {
    this_coro->ctx = std::move(ctx);
    return std::move(sched_ctx);
  }).resume();
}

void CoroBase::Terminate() {
  int tries = 0;
  while (!IsReturned()) {
    ++tries;
    Resume();
    assert(tries < 10000000 &&
           "coroutine is spinning too long, possible wrong terminating order");
  }
}

void Token::Reset() { parked = false; }

void Token::Park() {
  parked = true;
  CoroYield();
}

void Token::Unpark() { parked = false; }
