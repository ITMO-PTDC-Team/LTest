//
// Created by bitree.
//

#ifndef LTEST_FOLLY_CORO_AWAIT_ADAPTER_H
#define LTEST_FOLLY_CORO_AWAIT_ADAPTER_H

#pragma once

#include <cassert>
#include <coroutine>
#include <cstddef>
#include <deque>
#include <exception>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

#include <folly/CancellationToken.h>
#include <folly/Executor.h>
#include <folly/Try.h>
#include <folly/coro/Promise.h>
#include <folly/coro/Task.h>
#include <folly/coro/ViaIfAsync.h>
#include <folly/futures/Future.h>

#include "../../../runtime/include/verifying_macro.h"

class FollyLtestExecutor final : public folly::Executor {
 public:
  as_atomic void add(folly::Func func) override {
    queue_.push_back(std::move(func));
  }

  as_atomic void driveAll() {
    while (!queue_.empty()) {
      auto func = std::move(queue_.front());
      queue_.pop_front();
      std::move(func)();
    }
  }

 private:
  std::deque<folly::Func> queue_;
};

template <class Awaitable, class OnResume>
class FollyVoidAwaiterAdapter {
 public:
  using Awaiter = decltype(std::declval<Awaitable&&>().operator co_await());

  FollyVoidAwaiterAdapter(Awaitable&& awaitable, OnResume&& on_resume)
      : awaitable_(std::move(awaitable)),
        awaiter_(std::move(awaitable_).operator co_await()),
        on_resume_(std::move(on_resume)) {}

  FollyVoidAwaiterAdapter(const FollyVoidAwaiterAdapter&) = delete;
  FollyVoidAwaiterAdapter& operator=(const FollyVoidAwaiterAdapter&) = delete;

  as_atomic bool await_ready() noexcept(
      noexcept(std::declval<Awaiter&>().await_ready())) {
    return awaiter_.await_ready();
  }

  as_atomic decltype(auto) await_suspend(std::coroutine_handle<> h) noexcept(
      noexcept(std::declval<Awaiter&>().await_suspend(h))) {
    return awaiter_.await_suspend(h);
  }

  as_atomic void await_resume() noexcept(
      noexcept(std::declval<Awaiter&>().await_resume())) {
    awaiter_.await_resume();
    on_resume_();
  }

 private:
  Awaitable awaitable_;
  Awaiter awaiter_;
  OnResume on_resume_;
};

template <class Awaitable, class OnResume>
auto MakeFollyVoidAwaiterAdapter(Awaitable&& awaitable,
                                 OnResume&& on_resume) {
  using AwaitableT = std::decay_t<Awaitable>;
  using OnResumeT = std::decay_t<OnResume>;
  return FollyVoidAwaiterAdapter<AwaitableT, OnResumeT>(
      std::forward<Awaitable>(awaitable), std::forward<OnResume>(on_resume));
}

template <class T>
class FollyFutureAwaiterAdapter {
 public:
  using Future = folly::coro::Future<T>;
  using Awaiter = typename Future::WaitOperation;
  static constexpr bool ltest_cleanup_before_target_destroy = true;

  explicit FollyFutureAwaiterAdapter(Future&& future)
      : future_(folly::coro::co_withCancellation(
            cancel_source_.getToken(), std::move(future))) {}

  FollyFutureAwaiterAdapter(const FollyFutureAwaiterAdapter&) = delete;
  FollyFutureAwaiterAdapter& operator=(const FollyFutureAwaiterAdapter&) =
      delete;
  FollyFutureAwaiterAdapter(FollyFutureAwaiterAdapter&&) = delete;
  FollyFutureAwaiterAdapter& operator=(FollyFutureAwaiterAdapter&&) = delete;

  ~FollyFutureAwaiterAdapter() {
    ResetAwaiter();
  }

  as_atomic bool await_ready() noexcept(
      noexcept(std::declval<Awaiter&>().await_ready())) {
    Start();
    return get()->await_ready();
  }

  as_atomic decltype(auto) await_suspend(
      std::coroutine_handle<> continuation) noexcept(
      noexcept(std::declval<Awaiter&>().await_suspend(continuation))) {
    Start();
    bridge_state_ = std::make_shared<BridgeState>();
    bridge_state_->continuation = continuation;
    bridge_.emplace(MakeBridge(bridge_state_));
    return get()->await_suspend(bridge_->h);
  }

  as_atomic decltype(auto) await_resume() noexcept(
      noexcept(std::declval<Awaiter&>().await_resume())) {
    assert(engaged_);
    if constexpr (std::is_same_v<T, void>) {
      get()->await_resume();
      ResetAwaiter();
    } else {
      auto result = get()->await_resume();
      ResetAwaiter();
      return result;
    }
  }

  as_atomic void unregister() {
    if (bridge_state_) {
      bridge_state_->active = false;
    }
    cancel_source_.requestCancellation();
    if (engaged_ && get()->await_ready()) {
      ResetAwaiter();
    }
  }

 private:
  struct BridgeState {
    std::coroutine_handle<> continuation;
    bool active{true};
  };

  struct Bridge {
    struct promise_type {
      Bridge get_return_object() {
        return Bridge{
            std::coroutine_handle<promise_type>::from_promise(*this)};
      }
      std::suspend_always initial_suspend() noexcept { return {}; }
      std::suspend_always final_suspend() noexcept { return {}; }
      void return_void() noexcept {}
      void unhandled_exception() { std::terminate(); }
    };

    explicit Bridge(std::coroutine_handle<promise_type> handle) : h(handle) {}
    Bridge(Bridge&& other) noexcept : h(std::exchange(other.h, {})) {}
    Bridge& operator=(Bridge&&) = delete;
    Bridge(const Bridge&) = delete;
    Bridge& operator=(const Bridge&) = delete;
    ~Bridge() {
      if (h) {
        h.destroy();
      }
    }

    std::coroutine_handle<promise_type> h;
  };

  static as_atomic Bridge MakeBridge(std::shared_ptr<BridgeState> state) {
    if (state->active && state->continuation) {
      state->continuation.resume();
    }
    co_return;
  }

  Awaiter* get() {
    return std::launder(reinterpret_cast<Awaiter*>(storage_));
  }

  as_atomic void Start() {
    if (!engaged_) {
      ::new (storage_) Awaiter(std::move(future_).operator co_await());
      engaged_ = true;
    }
  }

  as_atomic void ResetAwaiter() {
    if (engaged_) {
      get()->~Awaiter();
      engaged_ = false;
    }
    bridge_.reset();
    bridge_state_.reset();
  }

  folly::CancellationSource cancel_source_;
  Future future_;
  alignas(Awaiter) std::byte storage_[sizeof(Awaiter)];
  bool engaged_{false};
  std::shared_ptr<BridgeState> bridge_state_;
  std::optional<Bridge> bridge_;
};

template <class T>
auto MakeFollyFutureAwaiterAdapter(folly::coro::Future<T>&& future) {
  return FollyFutureAwaiterAdapter<T>(std::move(future));
}

template <class T,
          bool EmitRequestBeforeSuspend = false,
          bool CancelOnUnregister = true,
          class OnUnregister = std::nullptr_t>
class FollyTaskAwaiter {
 public:
  static constexpr bool ltest_emit_request_before_suspend =
      EmitRequestBeforeSuspend;
  static constexpr bool ltest_cleanup_before_target_destroy = true;

  using Task = folly::coro::Task<T>;
  using StorageType = typename Task::StorageType;
  using Awaiter =
      decltype(folly::coro::co_viaIfAsync(
          std::declval<folly::Executor::KeepAlive<>>(), std::declval<Task&&>()));

  FollyTaskAwaiter(Task&& task, FollyLtestExecutor& executor,
                   OnUnregister on_unregister = {})
      : task_(folly::coro::co_withCancellation(
            cancel_source_.getToken(), std::move(task))),
        executor_(&executor),
        keep_alive_(folly::getKeepAliveToken(executor)),
        on_unregister_(std::move(on_unregister)) {}

  FollyTaskAwaiter(FollyTaskAwaiter&&) = delete;
  FollyTaskAwaiter& operator=(FollyTaskAwaiter&&) = delete;
  FollyTaskAwaiter(const FollyTaskAwaiter&) = delete;
  FollyTaskAwaiter& operator=(const FollyTaskAwaiter&) = delete;

  as_atomic bool await_ready() {
    Start();
    return awaiter_->await_ready();
  }

  as_atomic decltype(auto) await_suspend(std::coroutine_handle<> continuation) {
    Start();
    bridge_state_ = std::make_shared<BridgeState>();
    bridge_state_->continuation = continuation;
    bridge_.emplace(MakeBridge(bridge_state_));
    return awaiter_->await_suspend(bridge_->h);
  }

  as_atomic auto await_resume() {
    assert(awaiter_.has_value());
    if constexpr (std::is_same_v<T, void>) {
      awaiter_->await_resume();
      executor_->driveAll();
      awaiter_.reset();
      bridge_.reset();
    } else {
      auto result = awaiter_->await_resume();
      executor_->driveAll();
      awaiter_.reset();
      bridge_.reset();
      return result;
    }
  }

  as_atomic void ltest_drive_executor() {
    executor_->driveAll();
  }

  as_atomic void unregister() {
    if (bridge_state_) {
      bridge_state_->active = false;
    }
    if constexpr (!std::is_same_v<OnUnregister, std::nullptr_t>) {
      on_unregister_();
    }
    if constexpr (CancelOnUnregister) {
      cancel_source_.requestCancellation();
    }
    executor_->driveAll();

    if constexpr (!CancelOnUnregister) {
      if (awaiter_.has_value() && awaiter_->await_ready()) {
        awaiter_.reset();
        bridge_.reset();
      }
    }
  }

 private:
  struct BridgeState {
    std::coroutine_handle<> continuation;
    bool active{true};
  };

  struct Bridge {
    struct promise_type {
      Bridge get_return_object() {
        return Bridge{
            std::coroutine_handle<promise_type>::from_promise(*this)};
      }
      std::suspend_always initial_suspend() noexcept { return {}; }
      std::suspend_always final_suspend() noexcept { return {}; }
      void return_void() noexcept {}
      void unhandled_exception() { std::terminate(); }
    };

    explicit Bridge(std::coroutine_handle<promise_type> handle) : h(handle) {}
    Bridge(Bridge&& other) noexcept : h(std::exchange(other.h, {})) {}
    Bridge& operator=(Bridge&&) = delete;
    Bridge(const Bridge&) = delete;
    Bridge& operator=(const Bridge&) = delete;
    ~Bridge() {
      if (h) {
        h.destroy();
      }
    }

    std::coroutine_handle<promise_type> h;
  };

  static as_atomic Bridge MakeBridge(std::shared_ptr<BridgeState> state) {
    if (state->active && state->continuation) {
      state->continuation.resume();
    }
    co_return;
  }

  as_atomic void Start() {
    if (!awaiter_.has_value()) {
      awaiter_.emplace(
          folly::coro::co_viaIfAsync(keep_alive_, std::move(task_)));
    }
  }

 private:
  folly::CancellationSource cancel_source_;
  Task task_;
  FollyLtestExecutor* executor_;
  folly::Executor::KeepAlive<> keep_alive_;
  [[no_unique_address]] OnUnregister on_unregister_;
  std::optional<Awaiter> awaiter_;
  std::shared_ptr<BridgeState> bridge_state_;
  std::optional<Bridge> bridge_;
};

template <bool EmitRequestBeforeSuspend = false,
          bool CancelOnUnregister = true,
          class T,
          class OnUnregister = std::nullptr_t>
auto MakeFollyTaskViaIfAsync(folly::coro::Task<T>&& task,
                             FollyLtestExecutor& executor,
                             OnUnregister on_unregister = {}) {
  return FollyTaskAwaiter<T,
                          EmitRequestBeforeSuspend,
                          CancelOnUnregister,
                          OnUnregister>(
      std::move(task), executor, std::move(on_unregister));
}

#endif  // LTEST_FOLLY_CORO_AWAIT_ADAPTER_H
