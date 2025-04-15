#include "../specs/unique_args.h"

#include <cassert>
#include <cstring>
#include <functional>
#include <optional>


 std::vector<size_t> used(limit, false);
 std::vector<size_t> done(limit, false);

struct UniqueArgsTest {
  UniqueArgsTest() {}
  ValueWrapper Get(size_t i) {
    assert(!used[i]);
    used[i] = true;
    CoroYield();
    auto l = [this]() {
      Reset();
      return limit;
    };
    done[i] = true;
    return {std::count(done.begin(), done.end(), false) == 0 ? l() : std::optional<int>(),
            get_default_compator<std::optional<int>>(),
            print};
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

target_method(generateArgs, int, UniqueArgsTest, Get, size_t);

struct UniqueArgsOptionsOverride {
  static ltest::DefaultOptions GetOptions() {
    return {.threads = limit,
     .tasks = limit,
     .switches = 100000000,
     .rounds = 10000,
     .depth = 1,
     .forbid_all_same = false,
     .verbose = false,
     .strategy = "tla",
     .weights = ""};
  }
};

using spec_t = ltest::Spec<UniqueArgsTest, spec::UniqueArgsRef,
                           spec::UniqueArgsHash, spec::UniqueArgsEquals, UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT(spec_t);
