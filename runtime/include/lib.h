#pragma once
#include "fiber_context.h"
#include <cassert>
#include <cstdint>
#include <exception>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "block_manager.h"
#include "coro_ctx_guard.h"
#include "value_wrapper.h"

#define panic() assert(false)

extern "C" void CoroYield();
extern "C" void CoroutineStatusChange(char* coroutine, bool start);

namespace ltest {

struct CoroBase;
struct CoroutineStatus;

struct CoroutineStatus {
  std::string_view name;
  bool has_started;
};

extern bool ltest_initialized;

// Current executing coroutine.
extern std::shared_ptr<CoroBase> this_coro;
extern int this_thread_id;

// Scheduler context
extern context::fiber_context sched_ctx;

extern std::optional<CoroutineStatus> coroutine_status;

struct TestFailure : public std::exception {
  explicit TestFailure(std::string message) : msg(std::move(message)) {}
  const char* what() const noexcept override { return msg.c_str(); }

 private:
  std::string msg;
};

void SetTestFailure(std::string message);
bool HasTestFailure();
const std::string& GetTestFailureMessage();
void ClearTestFailure();

struct CoroBase : public std::enable_shared_from_this<CoroBase> {
  CoroBase(const CoroBase&) = delete;
  CoroBase(CoroBase&&) = delete;
  CoroBase& operator=(CoroBase&&) = delete;

  virtual std::shared_ptr<CoroBase> Restart(void* this_ptr) = 0;

  // Resume the coroutine to the next yield.
  void Resume(size_t resumed_thread_id);

  // Check if the coroutine is returned.
  bool IsReturned() const;

  void setWakeupCondition(std::function<bool()> cond);
  void clearWakeupCondition();
  bool hasWakeupCondition() const;
  bool checkWakeupCondition() const;

  // Returns task id.
  int GetId() const;

  virtual ValueWrapper GetRetVal() const;
  virtual void TerminateWith(const ValueWrapper& value);
  virtual std::string_view GetName() const;

  virtual std::vector<std::string> GetStrArgs() const = 0;
  virtual void* GetArgs() const = 0;

  std::shared_ptr<CoroBase> GetPtr();

  // Terminate the coroutine.
  void Terminate();
  void DiscardRoundResults();
  void DestroyContext();

  void SetBlocked(const BlockState& state) {
    fstate = state;
    block_manager.BlockOn(state, this);
  }

  BlockState GetBlockState() { return fstate; }

  bool IsBlocked() {
    if (!block_manager.IsBlocked(fstate, this)) {
      return false;
    }
    if (hasWakeupCondition()) {
      return !checkWakeupCondition();
    }
    return true;
  }

  // Dual metadata and task-local history buffer.
  void SetDual(bool v) { is_dual_task_ = v; }
  bool IsDual() const { return is_dual_task_; }

  enum class DualEventKind : std::uint8_t {
    RequestResponse = 0,
    FollowUpInvoke = 1,
    FollowUpResponse = 2,
  };

  struct DualEvent {
    DualEventKind kind;
    std::uint64_t seqno;
    ValueWrapper result;  // meaningful only for FollowUpResponse
  };

  // Dual wrappers observe request/follow-up boundaries while the task is
  // running. The scheduler drains these events after Resume() and appends them
  // to the global dual history.
  void EmitDualEvent(DualEventKind kind, ValueWrapper result = void_v);

  std::vector<DualEvent> DrainDualEvents();
  bool HasDualEvents() const { return !pending_dual_events_.empty(); }

  virtual ~CoroBase();

  context::fiber_context& GetCtx() { return ctx; }

 protected:
  CoroBase() = default;

  friend void ::CoroYield();

  template <typename Target, typename... Args>
  friend class Coro;

  int id{};
  ValueWrapper ret{};
  bool returned{false};

  // condition of wake up
  std::function<bool()> wakeup_condition_{nullptr};

  BlockState fstate{};
  std::string_view name;
  context::fiber_context ctx;

 private:
  bool is_dual_task_{false};
  std::vector<DualEvent> pending_dual_events_{};
};

template <typename Target, typename... Args>
struct Coro final : public CoroBase {
  using CoroF = std::function<ValueWrapper(Target*, Args...)>;
  using ArgsToStringsF =
      std::function<std::vector<std::string>(std::shared_ptr<void>)>;

  std::shared_ptr<CoroBase> Restart(void* this_ptr) override {
    SchedCtxGuard guard;
    assert(IsReturned());
    auto coro = New(func, this_ptr, args, args_to_strings, name, id);

    // Preserve dual marker across round reset / replay.
    // Otherwise dual tasks turn into non-dual after Restart(), breaking dual history.
    coro->SetDual(this->IsDual());

    return coro;
  }

  static std::shared_ptr<CoroBase> New(CoroF func, void* this_ptr,
                                       std::shared_ptr<void> args,
                                       ArgsToStringsF args_to_strings,
                                       std::string_view name, int task_id) {
    auto c = std::make_shared<Coro>();
    c->func = std::move(func);
    c->args = std::move(args);
    c->name = name;
    c->id = task_id;
    c->args_to_strings = std::move(args_to_strings);
    c->this_ptr = this_ptr;

    // Do not capture shared_ptr<Coro> in the fiber lambda.
    // Otherwise we create a reference cycle:
    //   Coro -> ctx -> lambda -> shared_ptr<Coro> -> Coro
    //
    // The task lifetime is already managed externally by shared_ptr<Task>.
    // The fiber only needs non-owning access to the object while it is alive.
    Coro* self = c.get();

    c->ctx = context::fiber_context([self](context::fiber_context&& ctx) {
        auto real_args = static_cast<std::tuple<Args...>*>(self->args.get());
        auto this_arg =
            std::tuple<Target*>{static_cast<Target*>(self->this_ptr)};
        try {
          self->ret =
              std::apply(self->func, std::tuple_cat(this_arg, *real_args));
        } catch (const TestFailure& ex) {
          SchedCtxGuard guard;
          SetTestFailure(ex.what());
          self->ret = void_v;
        } catch (...) {
          throw;
        }
        self->returned = true;
        ltest_coro_ctx = false;
        return std::move(ctx);
    });

    return c;
  }

  std::vector<std::string> GetStrArgs() const override {
    assert(args_to_strings != nullptr);
    return args_to_strings(args);
  }

  void* GetArgs() const override { return args.get(); }

 private:
  CoroF func;
  std::shared_ptr<void> args;
  std::function<std::vector<std::string>(std::shared_ptr<void>)> args_to_strings;
  void* this_ptr{};
};

using Task = std::shared_ptr<CoroBase>;

struct TaskBuilder {
  using BuilderFunc = std::function<Task(void*, size_t, int)>;
  TaskBuilder(std::string name, BuilderFunc func)
      : name(std::move(name)), builder_func(std::move(func)) {}

  const std::string& GetName() const { return name; }

  Task Build(void* this_ptr, size_t thread_id, int task_id) const {
    SchedCtxGuard guard;
    return builder_func(this_ptr, thread_id, task_id);
  }

 private:
  std::string name;
  BuilderFunc builder_func;
};

}  // namespace ltest
