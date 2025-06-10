#include <cassert>
#include <coroutine>
#include <cstring>
#include <functional>
#include <optional>
#include <string>

#include "../specs/unique_args.h"
#include "runtime/include/lib.h"

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
  void Add(Coroutine&& coro) {
    // list.push_back(coro);
    coro.resume();
  }
  SimpleAwaitable Wait() { return {}; }
  std::vector<Coroutine> list;
};

Coroutine DoWork(int i) {
  state[i]++;
  // std::cerr << "updated" << i << "\n";
  co_return;
}

Coroutine Work(int i) {
  Waiter w;
  w.Add(DoWork(i));
  co_await w.Wait();
  assert(state[i] == 1);
  co_return;
}

static std::string str;

extern "C" char* PrintInt(int i) {
  str = std::to_string(i);
  return str.data();
};
struct DynThreadsTest {
  DynThreadsTest() {}
  ValueWrapper Get(size_t i) {
    assert(!used[i]);
    used[i] = true;
    bool last = std::count(used.begin(), used.end(), true) == limit;
    auto coro = Work(i);
    coro.resume();
    auto l = [this]() {
      std::fill(used.begin(), used.end(), false);
      return limit;
    };
    return {last ? l() : std::optional<int>(),
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
