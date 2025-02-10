/**
 * ./build/verifying/targets/deadlock -v --tasks 5 --strategy rr
 * ./build/verifying/targets/deadlock -v --tasks 5 --strategy random
 *
 * It important to limit switches.
 * ./build/verifying/targets/deadlock -v --tasks 2 --strategy tla --rounds 100000 --switches 4
 */
#include <atomic>
#include <cstring>

#include "../lib/mutex.h"

// Test is implementation and the specification at the same time.
struct Test {
  Test() {}

  // Lock(odd) in parallel with Lock(even) causes deadlock.
  non_atomic void Lock(std::shared_ptr<Token> token, int v) {
    if (v % 2 == 0) {
      mu1.Lock(token);
      CoroYield();
      mu2.Lock(token);
      CoroYield();
    } else {
      mu2.Lock(token);
      CoroYield();
      mu1.Lock(token);
      CoroYield();
    }
    mu1.Unlock();
    mu2.Unlock();
  }

  void Reset() {
    mu1 = Mutex{};
    mu2 = Mutex{};
  }

  Mutex mu1, mu2{};

  using method_t = std::function<value_wrapper(Test *t, void *args)>;

  static auto GetMethods() {
    method_t lock_func = [](Test *l, void *args) -> int {
      // `void` return type is always return 0 equivalent.
      return 0;
    };

    return std::map<std::string, method_t>{
        {"Lock", lock_func},
    };
  }
};

auto generateInt(size_t thread_num) {
  return ltest::generators::makeSingleArg(static_cast<int>(thread_num));
}

auto generateArgs(size_t thread_num) {
  auto token = ltest::generators::genToken(thread_num);
  auto _int = generateInt(thread_num);
  return std::tuple_cat(token, _int);
}

target_method(generateArgs, void, Test, Lock, std::shared_ptr<Token>, int);

using spec_t = ltest::Spec<Test, Test>;

LTEST_ENTRYPOINT(spec_t);
