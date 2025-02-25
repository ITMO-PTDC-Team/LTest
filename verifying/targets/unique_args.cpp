#include "../specs/unique_args.h"

#include <cassert>
#include <cstring>
#include <functional>
#include <optional>
#include <set>


static std::set<size_t> used;
struct UniqueArgsTest {
  std::set<size_t> used_by_me_;
  UniqueArgsTest() {}
  ValueWrapper Get(size_t i) {
    used.insert(i);
    used_by_me_.insert(i);
    CoroYield();
    auto l = [this]() {
      Reset();
      return limit;
    };
    return {used.size() == limit ? l() : std::optional<int>(),
            value_wrapper_details::get_default_compator<std::optional<int>>(),
            print};
  }
  void Reset() {
    // std::cerr << "reset was called" <<"\n";
    for (auto &a : used_by_me_) {
      used.erase(a);
      // std::cerr << a << "\n";
    }
    used_by_me_.clear();
    std::cerr.flush();
  }
};

auto generateArgs(size_t thread_num) {
  assert(used.size() < limit);
  for (size_t i = 0; i < limit; i++) {
    if (!used.contains(i)) {
      used.insert(i);
      // std::cerr << "generated " << i <<"\n";
      std::cerr.flush();
      return ltest::generators::makeSingleArg(i);
    }
  }
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