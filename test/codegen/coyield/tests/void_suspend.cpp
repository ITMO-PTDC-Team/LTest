// RUN: %check
#include <coroutine>
struct SimpleAwaitable {
  bool await_ready() const noexcept { return false; }

  void await_suspend(std::coroutine_handle<> h) const noexcept { h.resume(); }

  void await_resume() const noexcept {}
};

struct CoroTask {
  struct promise_type {
    CoroTask get_return_object() { return {}; }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
  };
  auto operator co_await() const { return SimpleAwaitable{}; }
};

CoroTask myCoroutine2() { co_return; }
CoroTask myCoroutine() {
  // CHECK: call void @CoroutineStatusChange(ptr [[name:@[0-9]+]], i1 true)
  co_await myCoroutine2();
  // CHECK: call void @CoroutineStatusChange(ptr [[name]], i1 false)
}