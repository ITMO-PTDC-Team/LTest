#include <cassert>
#include <coroutine>
#include <cstring>
#include <functional>
#include <optional>

#include "../specs/unique_args.h"

struct Promise;

// NOLINTBEGIN(readability-identifier-naming)
struct Coroutine : std::coroutine_handle<Promise> {
  using promise_type = ::Promise;
};

struct Promise {
  Coroutine get_return_object() { return {Coroutine::from_promise(*this)}; }
  std::suspend_never initial_suspend() noexcept { return {}; }
  std::suspend_always final_suspend() noexcept { return {}; }
  void return_void() {}
  void unhandled_exception() {}
};
// NOLINTEND(readability-identifier-naming)

static std::vector<size_t> used(limit, false);
static std::vector<size_t> done(limit, false);

Coroutine CoFun(int i) {
  done[i] = true;
  co_return;
}
struct CoUniqueArgsTest {
  CoUniqueArgsTest() { Reset(); }
  ValueWrapper Get(size_t i) {
    assert(!used[i]);
    used[i] = true;
    auto l = [this]() {
      Reset();
      return limit;
    };
    CoFun(i);
    return {std::count(done.begin(), done.end(), false) == 0
                ? l()
                : std::optional<int>(),
            GetDefaultCompator<std::optional<int>>(), Print};
  }
  void Reset() {
    std::fill(used.begin(), used.end(), false);
    std::fill(done.begin(), done.end(), false);
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

target_method(GenerateArgs, int, CoUniqueArgsTest, Get, size_t);

using SpecT =
    ltest::Spec<CoUniqueArgsTest, spec::UniqueArgsRef, spec::UniqueArgsHash,
                spec::UniqueArgsEquals, spec::UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT(SpecT);
