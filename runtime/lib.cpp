#include "include/lib.h"

#include <cassert>
#include <deque>
#include <string>
#include <utility>
#include <vector>

#include "coro_ctx_guard.h"
#include "logger.h"
#include "value_wrapper.h"

#include <atomic>

// See comments in the lib.h.
Task this_coro{};
int this_thread_id = -1;

boost::context::fiber_context sched_ctx;
std::optional<CoroutineStatus> coroutine_status;
bool ltest_round_terminating = false;

namespace {
std::deque<std::coroutine_handle<>> g_external_resumes;
std::atomic<std::uint64_t> g_dual_event_seqno{0};
}  // namespace

namespace ltest {
std::vector<TaskBuilder> task_builders{};

void EnqueueExternalResume(std::coroutine_handle<> h) {
  if (h) {
    g_external_resumes.push_back(h);
  }
}

void DrainExternalResumes() {
  while (!g_external_resumes.empty()) {
    auto h = g_external_resumes.front();
    g_external_resumes.pop_front();
    if (h) {
      h.resume();
    }
  }
}
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

// ---- deferred cleanup / keepalive ----
void CoroBase::KeepAlive(std::shared_ptr<void> p) {
  if (!p) return;
  keepalive_.push_back(std::move(p));
}

void CoroBase::DeferDestroy(std::coroutine_handle<> h) {
  if (!h) return;
  deferred_destroy_.push_back(h);
}

void CoroBase::RunDeferredCleanup() {
  // Destroy deferred coroutine handles
  for (auto h : deferred_destroy_) {
    if (h) {
      h.destroy();
    }
  }
  deferred_destroy_.clear();

  // Release kept-alive heap objects
  keepalive_.clear();

  // (Optional safety) drop any pending dual events; at round end they should not matter.
  pending_dual_events_.clear();
}

void CoroBase::EmitDualEvent(DualEventKind kind, ValueWrapper result) {
  pending_dual_events_.push_back(
      DualEvent{kind, g_dual_event_seqno.fetch_add(1, std::memory_order_relaxed),
                std::move(result)});
}

std::vector<CoroBase::DualEvent> CoroBase::DrainDualEvents() {
  std::vector<DualEvent> out;
  out.swap(pending_dual_events_);
  return out;
}

void CoroBase::Resume(int resumed_thread_id) {
  ltest::DrainExternalResumes();
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
  ltest::DrainExternalResumes();
}

void CoroBase::setWakeupCondition(std::function<bool()> cond) {
  wakeup_condition_ = std::move(cond);
}

void CoroBase::clearWakeupCondition() { wakeup_condition_ = nullptr; }

bool CoroBase::hasWakeupCondition() const {
  return static_cast<bool>(wakeup_condition_);
}

bool CoroBase::checkWakeupCondition() const {
  return wakeup_condition_ ? wakeup_condition_() : false;
}

bool CoroBase::isReadyToRun() const {
  return !hasWakeupCondition() || checkWakeupCondition();
}

int CoroBase::GetId() const { return id; }

ValueWrapper CoroBase::GetRetVal() const {
  assert(IsReturned());
  return ret;
}

CoroBase::~CoroBase() {
  // Ensure no leaked coroutine handles if cleanup wasn't called explicitly.
  RunDeferredCleanup();

  // The coroutine must be returned if we want to restart it.
  // We can't just Terminate() it because it is the runtime responsibility to
  // decide, in which order the tasks should be terminated.
  assert(IsReturned() && "Task not returned at destruction");
}

std::string_view CoroBase::GetName() const { return name; }

bool CoroBase::IsReturned() const { return finish_kind_ != FinishKind::Running; }

CoroBase::FinishKind CoroBase::GetFinishKind() const {
  return finish_kind_;
}

bool CoroBase::FinishedNormally() const {
  return finish_kind_ == FinishKind::ReturnedNormally;
}

bool CoroBase::FinishedDuringTermination() const {
  return finish_kind_ == FinishKind::ReturnedDuringTermination;
}

void CoroBase::MarkFinishedNormally() {
  finish_kind_ = FinishKind::ReturnedNormally;
}

void CoroBase::MarkFinishedDuringTermination() {
  finish_kind_ = FinishKind::ReturnedDuringTermination;
}

void CoroBase::MarkFinishedNormallyIfRunning() {
  if (!IsReturned()) {
    MarkFinishedNormally();
  }
}

extern "C" void CoroYield() {
  if (!ltest_coro_ctx) [[unlikely]] {
    return;
  }
  //
  // ReleaseWithAser <-
  // shared_ptr<CoroBase>
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
  for (size_t i = 0; i < 1000 && !IsReturned(); ++i) {
    Resume(running_thread_id);
  }
}
