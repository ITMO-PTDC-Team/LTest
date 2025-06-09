// RUN: %check
#include <coroutine>
#include <string>
static std::string str;

extern "C" char* PrintInt(int i) {
  str = std::to_string(i);
  return str.data();
};

struct Promise;
// NOLINTBEGIN(readability-identifier-naming)
struct SimpleAwaitable {
  bool await_ready() const noexcept { return false; }

  bool await_suspend(std::coroutine_handle<> h) const noexcept {
    h.resume();
    return true;
  }

  void await_resume() const noexcept {}
};
struct Coroutine : std::coroutine_handle<Promise> {
  using promise_type = ::Promise;
  auto operator co_await() const { return SimpleAwaitable{}; }
};

struct Promise {
  Coroutine get_return_object() { return {Coroutine::from_promise(*this)}; }
  std::suspend_always initial_suspend() noexcept { return {}; }
  std::suspend_always final_suspend() noexcept { return {}; }
  void return_void() {}
  void unhandled_exception() {}
};
// NOLINTEND(readability-identifier-naming)

// Let's omit realization for simplicity
struct Waiter {
  void Add(Coroutine coro) {}
  SimpleAwaitable Wait() { return {}; }
};

Coroutine DoWork(int i) { co_return; }
Coroutine Work(int i) {
  Waiter w;
  // CHECK: call 
  w.Add(DoWork(i));
  // CHECK: call 
  co_await w.Wait();
}