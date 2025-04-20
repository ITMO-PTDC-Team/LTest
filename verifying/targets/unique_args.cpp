#include "../specs/unique_args.h"

#include <cassert>
#include <cstring>
#include <functional>
#include <optional>

static std::vector<size_t> used(limit, false);
static std::vector<size_t> done(limit, false);

struct CoUniqueArgsTest {
  CoUniqueArgsTest() {}
  ValueWrapper Get(size_t i) {
    assert(!used[i]);
    used[i] = true;
    CoroYield();
    auto l = [this]() {
      Reset();
      return limit;
    };
    done[i] = true;
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
      return ltest::generators::MakeSingleArg(i);
    }
  }
  assert(false && "extra call");
}

TARGET_METHOD(GenerateArgs, int, CoUniqueArgsTest, Get, size_t);

using SpecT =
    ltest::Spec<CoUniqueArgsTest, spec::UniqueArgsRef, spec::UniqueArgsHash,
                spec::UniqueArgsEquals, spec::UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT(SpecT);
