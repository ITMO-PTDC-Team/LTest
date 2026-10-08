#include "include/lib.h"

#include <atomic>
#include <cassert>
#include <cstdint>
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

boost::context::fiber sched_ctx;
bool ltest_initialized = false;

namespace {
std::atomic<std::uint64_t> g_dual_event_seqno{0};
}  // namespace

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
  {
    CoroCtxGuard guard;
    ctx = std::move(ctx).resume();
  }
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
  // Suspended coroutines are abandoned when the worker exits; never unwind
  // them to clean up an attempt. Only completed tasks may be destroyed.
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
  ltest::sched_ctx = std::move(ltest::sched_ctx).resume();
  ltest::ltest_coro_ctx = true;
}

extern "C" void CoroutineStatusChange(char*, bool) { CoroYield(); }

namespace ltest {

void CoroBase::Terminate() {
  // Keep a suspended fiber owned by this task until the worker calls _exit.
  returned = true;
  fstate = {};
  clearWakeupCondition();
}

void CoroBase::TerminateWith(const ValueWrapper& value) {
  Terminate();
  ret = value;
}

}  // namespace ltest
