#include <cassert>
#include <coroutine>
#include <cstring>
#include <functional>
#include <optional>

#include "../specs/unique_args.h"
#include "../specs/support_coro.h"


static std::vector<size_t> used(limit, false);
static std::vector<size_t> done(limit, false);

Coroutine CoWork(int i) {
  done[i] = true;
  co_return;
}

Coroutine CoFun(int i) {
  co_await CoWork(i);
}

struct NonLinearCommunicationTest {
  NonLinearCommunicationTest() {}
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

target_method(GenerateArgs, int, NonLinearCommunicationTest, Get, size_t);

using SpecT =
    ltest::Spec<NonLinearCommunicationTest, spec::UniqueArgsRef, spec::UniqueArgsHash,
                spec::UniqueArgsEquals, spec::UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT(SpecT);
