#include <cassert>
#include <coroutine>
#include <cstring>
#include <functional>
#include <optional>

#include "../specs/unique_args.h"

static std::vector<size_t> used(limit, false);
static std::vector<size_t> state(limit, 0);
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
  std::suspend_never final_suspend() noexcept { return {}; }
  void return_void() {}
  void unhandled_exception() {}
};
// NOLINTEND(readability-identifier-naming)

struct Waiter {
  void Add(const Coroutine& coro) {
    list.push_back(&coro);
  }
  SimpleAwaitable Wait() { 
    for (auto& a : list) {
      a->resume();
    }
    return {}; }
  std::vector<const Coroutine*> list;
};

Coroutine DoWork(int i) {
  state[i]++;
  return {};
}
Coroutine Work(int i) {
  Waiter w;
  w.Add(DoWork(i));
  state[i]++;
  co_await w.Wait();
}

struct DynThreadsTest {
  DynThreadsTest() {}
  ValueWrapper Get(size_t i) {
    assert(!used[i]);
    used[i] = true;
    auto coro= Work(i);
    coro.resume();
    auto l = [this]() {
      Reset();
      return limit;
    };
    return {std::count(state.begin(), state.end(), 2) == limit
                ? l()
                : std::optional<int>(),
            GetDefaultCompator<std::optional<int>>(), Print};
  }
  void Reset() {
    std::fill(used.begin(), used.end(), false);
    std::fill(state.begin(), state.end(), false);
  }
};

auto GenerateArgs(size_t thread_num) {
  for (size_t i = 0; i < limit; i++) {
    if (!used[i]) {
      return ltest::generators::makeSingleArg(i);
    }
  }
  assert(false && "extra call");
}

target_method(GenerateArgs, int, DynThreadsTest, Get, size_t);

using SpecT =
    ltest::Spec<DynThreadsTest, spec::UniqueArgsRef, spec::UniqueArgsHash,
                spec::UniqueArgsEquals, spec::UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT(SpecT);
