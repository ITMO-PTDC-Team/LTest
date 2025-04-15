#include <cassert>
#include <coroutine>
#include <cstring>
#include <functional>
#include <optional>

#include "../specs/unique_args.h"

struct promise;

struct coroutine : std::coroutine_handle<promise> {
  using promise_type = ::promise;
};

struct promise {
  coroutine get_return_object() { return {coroutine::from_promise(*this)}; }
  std::suspend_always initial_suspend() noexcept { return {}; }
  std::suspend_always final_suspend() noexcept { return {}; }
  void return_void() {}
  void unhandled_exception() {}
};

static std::vector<size_t> used(limit, false);
static std::vector<size_t> done(limit, false);

struct CoUniqueArgsTest {
  CoUniqueArgsTest() {}
  ValueWrapper Get(size_t i) {
    assert(!used[i]);
    used[i] = true;
    auto l = [this]() {
      Reset();
      return limit;
    };

    coroutine h = [](int i) -> coroutine {
      done[i] = true;
      co_return;
    }(i);
    h.resume();
    h.destroy();
    return {std::count(done.begin(), done.end(), false) == 0
                ? l()
                : std::optional<int>(),
            get_default_compator<std::optional<int>>(), print};
  }
  void Reset() {
    std::fill(used.begin(), used.end(), false);
    std::fill(done.begin(), done.end(), false);
  }
};

auto generateArgs(size_t thread_num) {
  for (size_t i = 0; i < limit; i++) {
    if (!used[i]) {
      return ltest::generators::makeSingleArg(i);
    }
  }
  assert(false && "extra call");
}

target_method(generateArgs, int, CoUniqueArgsTest, Get, size_t);

using spec_t =
    ltest::Spec<CoUniqueArgsTest, spec::UniqueArgsRef, spec::UniqueArgsHash,
                spec::UniqueArgsEquals, spec::UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT(spec_t);
