// Keeps as separated file because use in regression tests.
#pragma once

#include <cassert>
#include <concepts>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <new>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include "generators.h"
#include "lib.h"
#include "logger.h"
#include "value_wrapper.h"

namespace ltest {
extern std::vector<TaskBuilder> task_builders;

inline std::string FormatTestFailure(const char *expr, const char *file,
                                     unsigned int line, const char *func) {
  std::ostringstream oss;
  oss << "test failed: " << expr << " at " << file << ":" << line << " in "
      << func;
  return oss.str();
}

[[noreturn]] inline void LtestFail(const char *expr, const char *file,
                                   unsigned int line, const char *func) {
  SchedCtxGuard guard;
  throw TestFailure(FormatTestFailure(expr, file, line, func));
}
}  // namespace ltest

#define attr(attr) __attribute((__annotate__(#attr)))

#define non_atomic attr(ltest_nonatomic)
#define as_atomic attr(ltest_atomic)

#define rassert(expr) \
  ((expr) ? (void)0 : ltest::LtestFail(#expr, __FILE__, __LINE__, __FUNCTION__))

namespace ltest {

template <typename T>
std::string toString(const T& a);

template <typename T>
std::string toString(const T& a)
  requires(std::is_integral_v<T>)
{
  return std::to_string(a);
}

template <typename T>
std::string toString(const T &a)
  requires(std::is_pointer_v<T>)
{
  return "ptr";
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

template <typename Ret, typename Target, typename... Args>
struct MethodInvocation {
  using Method = std::function<ValueWrapper(Target*, Args...)>;

  inline static TaskBuilder GetTaskBuilder(std::string_view method_name,
                                           std::tuple<Args...> params,
                                           Method method) {
    auto builder =
        [method_name, params = std::move(params), method = std::move(method)](
            void* this_ptr, size_t unused_thread_num, int task_id) -> Task {
      auto args = std::shared_ptr<void>(new std::tuple(params));
      auto coro = Coro<Target, Args...>::New(method, this_ptr, args,
                                             &ltest::toStringArgs<Args...>,
                                             method_name, task_id);
      return coro;
    };
    return TaskBuilder(std::string(method_name), builder);
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

template <typename Target, typename... Args>
struct MethodInvocation<void, Target, Args...> {
  using Method = std::function<void(Target*, Args...)>;

  inline static TaskBuilder GetTaskBuilder(std::string_view method_name,
                                           std::tuple<Args...> params,
                                           Method method) {
    auto builder =
        [method_name, params = std::move(params), method = std::move(method)](
            void* this_ptr, size_t unused_thread_num, int task_id) -> Task {
      auto wrapper = [f = std::move(method)](void* this_ptr, Args&&... args) {
        f(reinterpret_cast<Target*>(this_ptr), std::forward<Args>(args)...);
        return void_v;
      };
      auto args = std::shared_ptr<void>(new std::tuple(params));
      auto coro = Coro<Target, Args...>::New(wrapper, this_ptr, args,
                                             &ltest::toStringArgs<Args...>,
                                             method_name, task_id);
      return coro;
    };
    return TaskBuilder(std::string(method_name), builder);
  }
};

template <class T>
concept HasAwaitMethods = requires(T t, std::coroutine_handle<> h) {
  { t.await_ready() } -> std::convertible_to<bool>;
  t.await_suspend(h);
  t.await_resume();
};

template <class T>
decltype(auto) ToAwaiter(T&& x) {
  using U = std::remove_reference_t<T>;
  if constexpr (HasAwaitMethods<U>) {
    return std::forward<T>(x);
  } else if constexpr (requires(U u) { std::move(u).operator co_await(); }) {
    return std::move(x).operator co_await();
  } else if constexpr (requires(U u) { u.operator co_await(); }) {
    return x.operator co_await();
  } else {
    static_assert(sizeof(U) == 0, "Return type is not awaiter/awaitable");
  }
}

template <class Awaiter>
constexpr bool EmitDualRequestBeforeSuspend() {
  if constexpr (requires {
                  { Awaiter::ltest_emit_request_before_suspend }
                      -> std::convertible_to<bool>;
                }) {
    return Awaiter::ltest_emit_request_before_suspend;
  } else {
    return false;
  }
}

// ------------------------------------
// Atomic helpers for dual wrapper.
//
// These functions access this_coro (shared_ptr) and block_manager — both of
// which contain load/store instructions.
// They MUST be as_atomic so that the yield pass does NOT insert
// CoroYield() inside them. Otherwise we get infinite recursion:
//   wrapper load → CoroYield → this_coro load → CoroYield → ...
// ------------------------------------
namespace detail {

as_atomic inline void dual_emit_request_done() {
  this_coro->EmitDualEvent(CoroBase::DualEventKind::RequestResponse, void_v);
}

as_atomic inline void dual_emit_followup_invoke() {
  this_coro->EmitDualEvent(CoroBase::DualEventKind::FollowUpInvoke, void_v);
}

as_atomic inline void dual_emit_followup_done(ValueWrapper res) {
  this_coro->EmitDualEvent(CoroBase::DualEventKind::FollowUpResponse,
                           std::move(res));
}

as_atomic inline void dual_set_blocked(const BlockState& state) {
  this_coro->SetBlocked(state);
}

template <class State>
as_atomic inline void dual_set_ready_wakeup_condition(
    std::shared_ptr<State> state) {
  this_coro->setWakeupCondition(
      [state = std::move(state)]() as_atomic -> bool { return state->ready; });
}

as_atomic inline void dual_clear_wakeup_condition() {
  this_coro->clearWakeupCondition();
}

as_atomic inline void dual_unblock_all(std::intptr_t addr) {
  block_manager.UnblockAllOn(addr);
}

template <class Awaiter>
as_atomic inline void dual_try_unregister(Awaiter& aw) {
  if constexpr (requires(Awaiter& x) { x.unregister(); }) {
    (void)aw.unregister();
  }
}

template <typename Ret, typename Awaiter>
inline ValueWrapper dual_finish_followup(Awaiter& aw) non_atomic {
  if constexpr (std::is_same_v<Ret, void>) {
    (void)aw.await_resume();
    {
      SchedCtxGuard guard;
      dual_emit_followup_done(void_v);
    }
    return void_v;
  } else {
    auto r = aw.await_resume();
    ValueWrapper vw{static_cast<Ret>(r)};
    {
      SchedCtxGuard guard;
      dual_emit_followup_done(vw);
    }
    return vw;
  }
}

}  // namespace detail

// ------------------------------------
// Ordinary async/awaitable target method
// ------------------------------------
template <typename Ret, typename Target, typename... Args>
struct TargetAwaitableMethod {
  template <typename MethodPtr>
  TargetAwaitableMethod(std::string_view method_name,
                        std::function<std::tuple<Args...>(size_t)> gen,
                        MethodPtr method_ptr) {
    auto wrapper = [method_ptr](Target* obj, Args... args) as_atomic
        -> ValueWrapper {
      auto run_with = [&](auto& aw) as_atomic -> ValueWrapper {
        bool ready = aw.await_ready();

        if (ready) {
          if constexpr (std::is_same_v<Ret, void>) {
            (void)aw.await_resume();
            return void_v;
          } else {
            return ValueWrapper(static_cast<Ret>(aw.await_resume()));
          }
        }

        struct WaitState {
          std::intptr_t addr;
          bool ready{false};
        };
        std::shared_ptr<WaitState> st;
        {
          SchedCtxGuard guard;
          st = std::make_shared<WaitState>();
          st->addr = reinterpret_cast<std::intptr_t>(st.get());
          detail::dual_set_ready_wakeup_condition(st);
        }

        struct Waker {
          struct promise_type {
            Waker get_return_object() {
              return {.h = std::coroutine_handle<promise_type>::from_promise(
                          *this)};
            }
            std::suspend_always initial_suspend() noexcept { return {}; }
            std::suspend_always final_suspend() noexcept { return {}; }
            void return_void() noexcept {}
            void unhandled_exception() { std::terminate(); }
          };
          std::coroutine_handle<promise_type> h;
        };

        auto make_waker =
            [](std::shared_ptr<WaitState> st) as_atomic -> Waker {
          st->ready = true;
          block_manager.UnblockAllOn(st->addr);
          co_return;
        };

        std::coroutine_handle<> h;
        {
          SchedCtxGuard guard;
          Waker w = make_waker(st);
          h = w.h;
        }
        bool suspended = true;
        using ASRet = decltype(aw.await_suspend(h));
        if constexpr (std::is_same_v<ASRet, bool>) {
          suspended = aw.await_suspend(h);
        } else if constexpr (std::is_same_v<ASRet, void>) {
          aw.await_suspend(h);
          suspended = true;
        } else if constexpr (std::is_convertible_v<ASRet,
                                                   std::coroutine_handle<>>) {
          std::coroutine_handle<> next = aw.await_suspend(h);
          if (next) {
            next.resume();
          }
          suspended = true;
        } else {
          static_assert(sizeof(ASRet) == 0,
                        "Unsupported await_suspend return type");
        }

        if (!suspended) {
          if constexpr (std::is_same_v<Ret, void>) {
            (void)aw.await_resume();
            return void_v;
          } else {
            return ValueWrapper(static_cast<Ret>(aw.await_resume()));
          }
        }

        while (true) {
          if constexpr (requires { aw.ltest_drive_executor(); }) {
            aw.ltest_drive_executor();
          }
          if (st->ready) {
            break;
          }

          detail::dual_set_blocked(BlockState{st->addr, 0});
          if (st->ready) {
            detail::dual_unblock_all(st->addr);
            break;
          }

          CoroYield();
        }

        assert(st->ready);
        detail::dual_clear_wakeup_condition();
        if constexpr (std::is_same_v<Ret, void>) {
          (void)aw.await_resume();
          return void_v;
        } else {
          return ValueWrapper(static_cast<Ret>(aw.await_resume()));
        }
      };

      using ReturnedT =
          std::remove_cvref_t<std::invoke_result_t<MethodPtr, Target*, Args...>>;

      if constexpr (HasAwaitMethods<ReturnedT>) {
        ReturnedT awaiter_obj(
            std::invoke(method_ptr, obj, std::forward<Args>(args)...));
        return run_with(awaiter_obj);
      } else {
        auto tmp = std::invoke(method_ptr, obj, std::forward<Args>(args)...);
        using AwaitableT = std::decay_t<decltype(tmp)>;
        AwaitableT awaitable(std::move(tmp));
        decltype(auto) aw = ToAwaiter(std::move(awaitable));
        using AwT = decltype(aw);

        if constexpr (std::is_lvalue_reference_v<AwT>) {
          return run_with(aw);
        } else {
          using AwaiterT = std::decay_t<AwT>;
          AwaiterT awaiter(std::move(aw));
          return run_with(awaiter);
        }
      }
    };

    auto builder = [gen = std::move(gen), method_name,
                    wrapper = std::move(wrapper)](
                       void* this_ptr, size_t thread_num, int task_id) -> Task {
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
// Supports both:
//  - awaiter (has await_* directly)
//  - awaitable (has operator co_await() returning awaiter), e.g. coro::task<T>
// ------------------------------------
template <typename Ret, typename Target, typename... Args>
struct TargetDualMethod {
  template <typename MethodPtr>
  TargetDualMethod(std::string_view method_name,
                   std::function<std::tuple<Args...>(size_t)> gen,
                   MethodPtr method_ptr) {
    auto wrapper = [method_ptr](Target* obj, Args... args) non_atomic
        -> ValueWrapper {
      // Core runner over a concrete awaiter object (has await_*).
      auto run_with = [&](auto& aw, bool emit_request_before_suspend) non_atomic
          -> ValueWrapper {
        bool ready = aw.await_ready();

        // ---- Immediate path ----
        if (ready) {
          {
            SchedCtxGuard guard;
            detail::dual_emit_request_done();
            detail::dual_emit_followup_invoke();
          }
          return detail::dual_finish_followup<Ret>(aw);
        }

        struct DualWaitState {
          std::intptr_t addr;
          bool ready{false};
        };
        std::shared_ptr<DualWaitState> st;
        {
          SchedCtxGuard guard;
          st = std::make_shared<DualWaitState>();
          st->addr = reinterpret_cast<std::intptr_t>(st.get());
          detail::dual_set_ready_wakeup_condition(st);
        }

        struct Waker {
          struct promise_type {
            Waker get_return_object() {
              return {.h = std::coroutine_handle<promise_type>::from_promise(
                          *this)};
            }
            std::suspend_always initial_suspend() noexcept { return {}; }
            std::suspend_always final_suspend() noexcept { return {}; }
            void return_void() noexcept {}
            void unhandled_exception() { std::terminate(); }
          };
          std::coroutine_handle<promise_type> h;
        };

        auto make_waker = [](std::shared_ptr<DualWaitState> st) as_atomic
            -> Waker {
          st->ready = true;
          block_manager.UnblockAllOn(st->addr);
          co_return;
        };

        std::coroutine_handle<> h;
        {
          SchedCtxGuard guard;
          Waker w = make_waker(st);
          h = w.h;
        }

        using ASRet = decltype(aw.await_suspend(h));
        const bool request_before_suspend = emit_request_before_suspend;

        // Try to suspend (support await_suspend returning: bool / void / coroutine_handle<>)
        bool suspended = true;

        // Some awaiters require the request edge to be visible before calling
        // await_suspend(). Awaiters that know this is required opt in with
        // ltest_emit_request_before_suspend.
        if (request_before_suspend) {
          SchedCtxGuard guard;
          detail::dual_emit_request_done();
        }

        if constexpr (std::is_same_v<ASRet, bool>) {
          suspended = aw.await_suspend(h);
        } else if constexpr (std::is_same_v<ASRet, void>) {
          aw.await_suspend(h);
          suspended = true;
        } else if constexpr (std::is_convertible_v<ASRet,
                                                   std::coroutine_handle<>>) {
          std::coroutine_handle<> next = aw.await_suspend(h);
          if (next) {
            next.resume();
          }
          suspended = true;
        } else {
          static_assert(sizeof(ASRet) == 0,
                        "Unsupported await_suspend return type");
        }

        if (!suspended) {
          // No waiting => complete now.
          {
            SchedCtxGuard guard;
            if (!request_before_suspend) {
              detail::dual_emit_request_done();
            }
            detail::dual_emit_followup_invoke();
          }
          return detail::dual_finish_followup<Ret>(aw);
        }

        // Waiting => request phase finished now.
        if (!request_before_suspend) {
          SchedCtxGuard guard;
          detail::dual_emit_request_done();
        }

        // Block current LTest task until waker runs.
        while (true) {
          if constexpr (requires { aw.ltest_drive_executor(); }) {
            aw.ltest_drive_executor();
          }
          if (st->ready) break;

          {
            SchedCtxGuard guard;
            detail::dual_set_blocked(BlockState{st->addr, 0});

            // If waker raced with SetBlocked() (woke before we enqueued),
            // remove ourselves from the queue.
            if (st->ready) {
              detail::dual_unblock_all(st->addr);
              break;
            }
          }

          CoroYield();
        }

        assert(st->ready);
        {
          SchedCtxGuard guard;
          detail::dual_clear_wakeup_condition();
          detail::dual_emit_followup_invoke();
        }
        return detail::dual_finish_followup<Ret>(aw);
      };

      using ReturnedT =
          std::remove_cvref_t<std::invoke_result_t<MethodPtr, Target*, Args...>>;

      if constexpr (HasAwaitMethods<ReturnedT>) {
        ReturnedT awaiter_obj(
            std::invoke(method_ptr, obj, std::forward<Args>(args)...));
        return run_with(awaiter_obj,
                        EmitDualRequestBeforeSuspend<ReturnedT>());
      } else {
        auto tmp = std::invoke(method_ptr, obj, std::forward<Args>(args)...);
        using AwaitableT = std::decay_t<decltype(tmp)>;
        AwaitableT awaitable(std::move(tmp));
        decltype(auto) aw = ToAwaiter(std::move(awaitable));
        using AwT = decltype(aw);

        if constexpr (std::is_lvalue_reference_v<AwT>) {
          using AwaiterT = std::remove_reference_t<AwT>;
          return run_with(aw, EmitDualRequestBeforeSuspend<AwaiterT>());
        } else {
          using AwaiterT = std::decay_t<AwT>;
          AwaiterT awaiter(std::move(aw));
          return run_with(awaiter, EmitDualRequestBeforeSuspend<AwaiterT>());
        }
      }
    };

    auto builder = [gen = std::move(gen), method_name,
                    wrapper = std::move(wrapper)](
                       void* this_ptr, size_t thread_num, int task_id) -> Task {
      auto args = std::shared_ptr<void>(new std::tuple(gen(thread_num)));
      auto coro = Coro<Target, Args...>::New(wrapper, this_ptr, args,
                                             &ltest::toStringArgs<Args...>,
                                             method_name, task_id);
      coro->SetDual(true);
      return coro;
    };

    task_builders.push_back(
        TaskBuilder(std::string(method_name), builder));
  }
};

}  // namespace ltest

#define LTEST_REGISTRATION_GLOBAL \
  __attribute__((section("ltest_registration_globals")))

#define declare_task_name(symbol)                                      \
  static const char* symbol##_task_name LTEST_REGISTRATION_GLOBAL = #symbol

#define target_method(gen, ret, cls, symbol, ...)          \
  declare_task_name(symbol);                               \
  ltest::TargetMethod<ret, cls __VA_OPT__(, ) __VA_ARGS__>              \
      symbol##_ltest_method_cls LTEST_REGISTRATION_GLOBAL{             \
          symbol##_task_name, gen, &cls::symbol};

#define stringify_detail(x) #x
#define stringify(x) stringify_detail(x)
#define method_invocation(params, ret, cls, symbol, ...)                      \
  ltest::MethodInvocation<                                                    \
      ret, cls __VA_OPT__(, ) __VA_ARGS__>::GetTaskBuilder(stringify(symbol), \
                                                           params,            \
                                                           &cls::symbol)

#define target_method_async(gen, ret, cls, symbol, ...)              \
  declare_task_name(symbol);                                          \
  ltest::TargetAwaitableMethod<ret, cls __VA_OPT__(, ) __VA_ARGS__>     \
      symbol##_ltest_async_method_cls LTEST_REGISTRATION_GLOBAL{       \
          symbol##_task_name, gen, &cls::symbol};

#define target_method_dual(gen, ret, cls, symbol, ...)         \
  declare_task_name(symbol);                                   \
  ltest::TargetDualMethod<ret, cls __VA_OPT__(, ) __VA_ARGS__>          \
      symbol##_ltest_dual_method_cls LTEST_REGISTRATION_GLOBAL{        \
          symbol##_task_name, gen, &cls::symbol};
