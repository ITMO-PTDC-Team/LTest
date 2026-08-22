#include "include/lib.h"

#include <atomic>
#include <cassert>
#include <cstdint>
#include <new>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "coro_ctx_guard.h"
#include "logger.h"
#include "value_wrapper.h"

namespace ltest {

// See comments in the lib.h.
Task this_coro{};
int this_thread_id = -1;

context::fiber_context sched_ctx;
std::optional<CoroutineStatus> coroutine_status;
bool ltest_initialized = false;

namespace {
std::atomic<std::uint64_t> g_dual_event_seqno{0};
std::unordered_map<void*, size_t> fiber_stacks;
}  // namespace

boost::context::stack_context context::AllocateFiberStack() {
  SchedCtxGuard guard;
  const size_t size = boost::context::stack_traits::default_size();
  void* const memory = ::operator new(size);
  fiber_stacks.emplace(memory, size);
  return {.size = size, .sp = static_cast<char*>(memory) + size};
}

void context::DeallocateFiberStack(
    boost::context::stack_context& context) noexcept {
  SchedCtxGuard guard;
  void* const memory = static_cast<char*>(context.sp) - context.size;
  fiber_stacks.erase(memory);
  ::operator delete(memory);
}

std::size_t context::FreeForgottenFiberStacks() noexcept {
  SchedCtxGuard guard;
  std::unordered_map<void*, size_t> forgotten_stacks;
  forgotten_stacks.swap(fiber_stacks);
  const std::size_t count = forgotten_stacks.size();
  for (const auto& [memory, size] : forgotten_stacks) {
    (void)size;
    ::operator delete(memory);
  }
  return count;
}

std::vector<TaskBuilder> task_builders{};

// Test failure tracking for litmus tests, which could expectedly fail.
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

Task CoroBase::GetPtr() { return shared_from_this(); }

void CoroBase::EmitDualEvent(DualEventKind kind, ValueWrapper result) {
  SchedCtxGuard guard;
  pending_dual_events_.push_back(
      DualEvent{kind, g_dual_event_seqno.fetch_add(1, std::memory_order_relaxed),
                std::move(result)});
}

std::vector<CoroBase::DualEvent> CoroBase::DrainDualEvents() {
  std::vector<DualEvent> out;
  out.swap(pending_dual_events_);
  return out;
}

void CoroBase::Resume(size_t resumed_thread_id) {
  this_coro = this->GetPtr();
  this_thread_id = resumed_thread_id;
  assert(!this_coro->IsReturned() && this_coro->ctx);
  // debug(stderr, "name: %s\n",
  // std::string(this_coro->GetPtr()->GetName()).c_str());
  auto coro = this_coro.get();  // std::shared_ptr also can be interleaved
  // NOTE(kmitkin): Guard below prevents us from call CoroYield in the scheduler
  // coroutine, area that protected by it should be as small as possible to
  // reduce errors
  auto returned_context = context::fiber_context(
      [coro](context::fiber_context&& ctx) {
        sched_ctx = std::move(ctx);
        {
          CoroCtxGuard guard{};
          coro->ctx = std::move(coro->ctx).resume();
        }
        return std::move(sched_ctx);
      }).resume();
  returned_context.forget();
  this_coro.reset();
  this_thread_id = -1;
}

void CoroBase::setWakeupCondition(std::function<bool()> cond) {
  SchedCtxGuard guard;
  wakeup_condition_ = std::move(cond);
}

void CoroBase::clearWakeupCondition() {
  SchedCtxGuard guard;
  wakeup_condition_ = nullptr;
}

bool CoroBase::hasWakeupCondition() const {
  return static_cast<bool>(wakeup_condition_);
}

bool CoroBase::checkWakeupCondition() const {
  return wakeup_condition_ ? wakeup_condition_() : false;
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
  assert(IsReturned() && "Task not returned at destruction");
}

std::string_view CoroBase::GetName() const { return name; }

bool CoroBase::IsReturned() const { return returned; }

}  // namespace ltest

extern "C" void CoroYield() {
  if (!ltest::ltest_coro_ctx || ltest::ltest_target_construction) [[unlikely]] {
    return;
  }
  assert(ltest::this_coro && ltest::sched_ctx);
  ltest::ltest_coro_ctx = false;
  auto returned_context = ltest::context::fiber_context(
      [](ltest::context::fiber_context&& ctx) {
        ltest::this_coro->ctx = std::move(ctx);
        return std::move(ltest::sched_ctx);
      }).resume();
  returned_context.forget();
  ltest::ltest_coro_ctx = true;
}

extern "C" void CoroutineStatusChange(char* name, bool start) {
  // assert(!coroutine_status.has_value());
  ltest::coroutine_status.emplace(name, start);
  CoroYield();
}

namespace ltest {

void CoroBase::DestroyContext() {
  if (ctx) {
    ctx.forget();
  }
}

void CoroBase::Terminate() {
  returned = true;
  fstate = {};
  clearWakeupCondition();
  DestroyContext();
}

void CoroBase::DiscardRoundResults() {
  SchedCtxGuard guard;
  ret = {};
  pending_dual_events_.clear();
}

void CoroBase::TerminateWith(const ValueWrapper& value) {
  Terminate();
  ret = value;
}

}  // namespace ltest
