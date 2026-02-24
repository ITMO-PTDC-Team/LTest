#pragma once
#include <boost/context/fiber.hpp>
#include <cassert>
#include <coroutine>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "block_manager.h"
#include "value_wrapper.h"

#define panic() assert(false)

struct CoroBase;
struct CoroutineStatus;

// Current executing coroutine.
extern std::shared_ptr<CoroBase> this_coro;

// Scheduler context
extern boost::context::fiber_context sched_ctx;

// True while runtime is terminating current round / cleaning up tasks.
// Used to let blocked dual operations exit without waiting for a match.
extern bool ltest_round_terminating;

extern std::optional<CoroutineStatus> coroutine_status;

struct CoroutineStatus {
  std::string_view name;
  bool has_started;
};

extern "C" void CoroYield();
extern "C" void CoroutineStatusChange(char* coroutine, bool start);

struct CoroBase : public std::enable_shared_from_this<CoroBase> {
  CoroBase(const CoroBase&) = delete;
  CoroBase(CoroBase&&) = delete;
  CoroBase& operator=(CoroBase&&) = delete;

  virtual std::shared_ptr<CoroBase> Restart(void* this_ptr) = 0;

  void Resume();
  bool IsReturned() const;

  void setWakeupCondition(std::function<bool()> cond);
  void clearWakeupCondition();
  bool hasWakeupCondition() const;
  bool checkWakeupCondition() const;
  bool isReadyToRun() const;

  // Returns task id.
  int GetId() const;

  virtual ValueWrapper GetRetVal() const;
  virtual std::string_view GetName() const;

  virtual std::vector<std::string> GetStrArgs() const = 0;
  virtual void* GetArgs() const = 0;

  std::shared_ptr<CoroBase> GetPtr();

  void TryTerminate();
  void Terminate();

  void SetBlocked(const BlockState& state) {
    fstate = state;
    block_manager.BlockOn(state, this);
  }

  BlockState GetBlockState() { return fstate; }

  // NOTE: you already modified IsBlocked to depend on wakeup condition.
  bool IsBlocked() {
    return block_manager.IsBlocked(fstate, this) && isReadyToRun();
  }

  bool IsParked() const;

  // ---- dual metadata + pending dual events ----
  void SetDual(bool v) { is_dual_task_ = v; }
  bool IsDual() const { return is_dual_task_; }

  enum class DualEventKind : std::uint8_t {
    RequestResponse = 0,
    FollowUpInvoke = 1,
    FollowUpResponse = 2,
  };

  struct DualEvent {
    DualEventKind kind;
    ValueWrapper result;  // meaningful only for FollowUpResponse
  };

  // Called from inside dual wrapper (verifying_macro.h)
  void EmitDualEvent(DualEventKind kind, ValueWrapper result = void_v);

  // Called from scheduler (next step) to append to history
  std::vector<DualEvent> DrainDualEvents();
  bool HasDualEvents() const { return !pending_dual_events_.empty(); }

  // ---- lifetime management for dual termination safety ----
  //
  // KeepAlive: hold arbitrary heap state until end of round (e.g. awaitable object).
  // DeferDestroy: postpone coroutine_handle<>::destroy() until end of round.
  //
  void KeepAlive(std::shared_ptr<void> p);

  template <class T>
  void KeepAlive(std::shared_ptr<T> p) {
    KeepAlive(std::static_pointer_cast<void>(std::move(p)));
  }

  void DeferDestroy(std::coroutine_handle<> h);

  // Must be called when no further target code can resume deferred handles.
  // Safe to call multiple times.
  void RunDeferredCleanup();

  virtual ~CoroBase();

  boost::context::fiber_context& GetCtx() { return ctx; }

 protected:
  CoroBase() = default;

  friend void ::CoroYield();

  template <typename Target, typename... Args>
  friend class Coro;

  int id{};
  ValueWrapper ret{};
  bool is_returned{};

  // condition of wake up
  std::function<bool()> wakeup_condition_{nullptr};

  BlockState fstate{};
  std::string_view name;
  boost::context::fiber_context ctx;

 private:
  bool is_dual_task_{false};
  std::vector<DualEvent> pending_dual_events_{};

  // Keep heap objects alive until round end (awaitables, shared state, etc.)
  std::vector<std::shared_ptr<void>> keepalive_{};

  // Destroy coroutine handles at round end (waker handles passed to await_suspend()).
  std::vector<std::coroutine_handle<>> deferred_destroy_{};
};

template <typename Target, typename... Args>
struct Coro final : public CoroBase {
  using CoroF = std::function<ValueWrapper(Target*, Args...)>;
  using ArgsToStringsF =
      std::function<std::vector<std::string>(std::shared_ptr<void>)>;

  std::shared_ptr<CoroBase> Restart(void* this_ptr) override {
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
    c->ctx =
        boost::context::fiber_context([c](boost::context::fiber_context&& ctx) {
          auto real_args =
              reinterpret_cast<std::tuple<Args...>*>(c->args.get());
          auto this_arg =
              std::tuple<Target*>{reinterpret_cast<Target*>(c->this_ptr)};
          c->ret = std::apply(c->func, std::tuple_cat(this_arg, *real_args));
          c->is_returned = true;
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
    return builder_func(this_ptr, thread_id, task_id);
  }

 private:
  std::string name;
  BuilderFunc builder_func;
};