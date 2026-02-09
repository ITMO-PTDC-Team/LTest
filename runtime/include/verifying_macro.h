// Keeps as separated file because use in regression tests.
#pragma once
#include <cassert>
#include <coroutine>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include "generators.h"
#include "lib.h"
#include "value_wrapper.h"

namespace ltest {
extern std::vector<TaskBuilder> task_builders;
}  // namespace ltest

// Adds an attribute.
#define attr(attr) __attribute((__annotate__(#attr)))

// Tell that the function need to be converted to the coroutine.
#define non_atomic attr(ltest_nonatomic)
// Tell that the function must not contain interleavings.
#define as_atomic attr(ltest_atomic)

namespace ltest {

template <typename T>
std::string toString(const T& a);

template <typename T>
std::string toString(const T& a)
  requires(std::is_integral_v<T>)
{
  return std::to_string(a);
}

template <typename tuple_t, size_t... index>
auto toStringListHelper(const tuple_t& t,
                        std::index_sequence<index...>) noexcept {
  return std::vector<std::string>{ltest::toString(std::get<index>(t))...};
}

template <typename tuple_t>
auto toStringList(const tuple_t& t) noexcept {
  using tuple_type = typename std::remove_reference<decltype(t)>::type;
  constexpr auto s = std::tuple_size<tuple_type>::value;
  if constexpr (s == 0) {
    return std::vector<std::string>{};
  }
  return toStringListHelper<tuple_type>(t, std::make_index_sequence<s>{});
}

template <typename... Args>
auto toStringArgs(std::shared_ptr<void> args) {
  auto real_args = reinterpret_cast<std::tuple<Args...>*>(args.get());
  return toStringList(*real_args);
}

// ------------------------------------
// Regular (non-dual) target_method
// ------------------------------------
template <typename Ret, typename Target, typename... Args>
struct TargetMethod {
  using Method = std::function<ValueWrapper(Target*, Args...)>;
  TargetMethod(std::string_view method_name,
               std::function<std::tuple<Args...>(size_t)> gen, Method method) {
    auto builder = [gen = std::move(gen), method_name,
                    method = std::move(method)](
                       void* this_ptr, size_t thread_num, int task_id) -> Task {
      auto args = std::shared_ptr<void>(new std::tuple(gen(thread_num)));
      auto coro = Coro<Target, Args...>::New(method, this_ptr, args,
                                             &ltest::toStringArgs<Args...>,
                                             method_name, task_id);
      return coro;
    };
    ltest::task_builders.push_back(
        TaskBuilder(std::string(method_name), builder));
  }
};

template <typename Target, typename... Args>
struct TargetMethod<void, Target, Args...> {
  using Method = std::function<void(Target*, Args...)>;

  TargetMethod(std::string_view method_name,
               std::function<std::tuple<Args...>(size_t)> gen, Method method) {
    auto builder = [gen = std::move(gen), method_name,
                    method = std::move(method)](
                       void* this_ptr, size_t thread_num, int task_id) -> Task {
      auto wrapper = [f = std::move(method)](void* this_ptr, Args&&... args) {
        f(reinterpret_cast<Target*>(this_ptr), std::forward<Args>(args)...);
        return void_v;
      };
      auto args = std::shared_ptr<void>(new std::tuple(gen(thread_num)));
      auto coro = Coro<Target, Args...>::New(wrapper, this_ptr, args,
                                             &ltest::toStringArgs<Args...>,
                                             method_name, task_id);
      return coro;
    };
    ltest::task_builders.push_back(
        TaskBuilder(std::string(method_name), builder));
  }
};

// ------------------------------------
// Dual target_method_dual (emit events into task buffer)
// ------------------------------------
template <typename Ret, typename Target, typename... Args>
struct TargetDualMethod {
  template <typename MethodPtr>
  TargetDualMethod(std::string_view method_name,
                   std::function<std::tuple<Args...>(size_t)> gen,
                   MethodPtr method_ptr) {
    auto wrapper = [method_ptr](Target* obj, Args... args) -> ValueWrapper {
      // Cleanup path: do not start a new dual wait during round termination.
      if (ltest_round_terminating) {
        if constexpr (std::is_same_v<Ret, void>) {
          return void_v;
        } else {
          return ValueWrapper(Ret{});
        }
      }

      auto awaitable =
          std::invoke(method_ptr, obj, std::forward<Args>(args)...);

      // Helper to emit RequestResponse + FollowUpInvoke exactly once.
      auto emit_request_done = []() {
        assert(this_coro);
        this_coro->EmitDualEvent(CoroBase::DualEventKind::RequestResponse,
                                 void_v);
        this_coro->EmitDualEvent(CoroBase::DualEventKind::FollowUpInvoke,
                                 void_v);
      };

      // Helper to emit FollowUpResponse(result)
      auto emit_followup_done = [](ValueWrapper res) {
        assert(this_coro);
        this_coro->EmitDualEvent(CoroBase::DualEventKind::FollowUpResponse,
                                 std::move(res));
      };

      // Immediate path.
      // TODO(bitree): записать сюда контекст

      if (awaitable.await_ready()) {
        emit_request_done();
        if constexpr (std::is_same_v<Ret, void>) {
          (void)awaitable.await_resume();
          emit_followup_done(void_v);
          return void_v;
        } else {
          auto r = awaitable.await_resume();
          ValueWrapper vw{static_cast<Ret>(r)};
          emit_followup_done(vw);
          return vw;
        }
      }

      // Shared state used to block/unblock current LTest task.
      struct DualWaitState {
        std::intptr_t addr;
        bool ready{false};
      };
      auto st = std::make_shared<DualWaitState>();
      st->addr = reinterpret_cast<std::intptr_t>(st.get());

      // Small coroutine that is resumed by the matching operation.
      // It unblocks current LTest task.
      struct Waker {
        struct promise_type {
          Waker get_return_object() {
            return {.h = std::coroutine_handle<promise_type>::from_promise(*this)};
          }
          std::suspend_always initial_suspend() noexcept { return {}; }
          std::suspend_always final_suspend() noexcept { return {}; }
          void return_void() noexcept {}
          void unhandled_exception() { std::terminate(); }
        };
        std::coroutine_handle<promise_type> h;
      };

      auto make_waker = [](std::shared_ptr<DualWaitState> st) -> Waker {
        st->ready = true;
        block_manager.UnblockAllOn(st->addr);
        co_return;
      };
      /*
       * T0: request done. T0 -> unblocking
       *
       * T1: request done.
       */

      Waker w = make_waker(st);
      std::coroutine_handle<> h = w.h;

      // Try to suspend.
      bool suspended = awaitable.await_suspend(h);
      if (!suspended) {
        // No waiting => complete now.
        if (h) h.destroy();
        emit_request_done();
        if constexpr (std::is_same_v<Ret, void>) {
          (void)awaitable.await_resume();
          emit_followup_done(void_v);
          return void_v;
        } else {
          auto r = awaitable.await_resume();
          ValueWrapper vw{static_cast<Ret>(r)};
          emit_followup_done(vw);
          return vw;
        }
      }

      // Waiting => request phase is finished now.
      emit_request_done();

      // Block current LTest task until waker runs.
      while (!st->ready && !ltest_round_terminating) {
        assert(this_coro && "dual wrapper called outside of task context");
        this_coro->SetBlocked(BlockState{st->addr, 0});
        CoroYield();
      }

      // Cleanup waker coroutine.
      if (h) h.destroy();

      // Termination cleanup path: exit without await_resume() and without FollowUpResponse.
      if (ltest_round_terminating) {
        if constexpr (std::is_same_v<Ret, void>) {
          return void_v;
        } else {
          return ValueWrapper(Ret{});
        }
      }

      // Normal completion.
      assert(st->ready);
      if constexpr (std::is_same_v<Ret, void>) {
        (void)awaitable.await_resume();
        emit_followup_done(void_v);
        return void_v;
      } else {
        auto r = awaitable.await_resume();
        ValueWrapper vw{static_cast<Ret>(r)};
        emit_followup_done(vw);
        return vw;
      }
    };

    auto builder = [gen = std::move(gen), method_name,
                    wrapper = std::move(wrapper)](
                       void* this_ptr, size_t thread_num, int task_id) -> Task {
      auto args = std::shared_ptr<void>(new std::tuple(gen(thread_num)));
      auto coro = Coro<Target, Args...>::New(wrapper, this_ptr, args,
                                             &ltest::toStringArgs<Args...>,
                                             method_name, task_id);
      // Mark this task as dual so scheduler (later) can treat it specially.
      coro->SetDual(true);
      return coro;
    };

    ltest::task_builders.push_back(
        TaskBuilder(std::string(method_name), builder));
  }
};

}  // namespace ltest

#define declare_task_name(symbol) \
  static const char* symbol##_task_name = #symbol

#define target_method(gen, ret, cls, symbol, ...)          \
  declare_task_name(symbol);                               \
  ltest::TargetMethod<ret, cls __VA_OPT__(, ) __VA_ARGS__> \
      symbol##_ltest_method_cls{symbol##_task_name, gen, &cls::symbol}

#define target_method_dual(gen, ret, cls, symbol, ...)          \
  declare_task_name(symbol);                                    \
  ltest::TargetDualMethod<ret, cls __VA_OPT__(, ) __VA_ARGS__>  \
      symbol##_ltest_dual_method_cls{symbol##_task_name, gen, &cls::symbol}