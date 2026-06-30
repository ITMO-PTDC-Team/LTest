//
// Created by bitree.
//

#include "../../specs/folly/coro_shared_promise.h"
#include "folly_coro_await_adapter.h"

#include "../../../runtime/include/verifying.h"

#include <cstdlib>

#include <folly/io/async/Request.h>
#include <folly/coro/SharedPromise.h>

extern "C" const char* __asan_default_options() {
  return "detect_leaks=0";
}

static auto genPositiveInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

struct FollyCoroSharedPromiseTarget {
  FollyLtestExecutor ex;
  folly::coro::SharedPromise<int> promise;

  FollyCoroSharedPromiseTarget() {
    WarmRequestContext();
  }

  static as_atomic void WarmRequestContext() {
    (void)folly::RequestContext::get();
  }

  static folly::coro::Task<int> GetValue(FollyCoroSharedPromiseTarget* self) {
    co_return co_await self->promise.getFuture();
  }

  non_atomic auto get() {
    return MakeFollyTaskViaIfAsync(GetValue(this), ex);
  }

  non_atomic bool set_value(int value) {
    try {
      promise.setValue(value);
      return true;
    } catch (const folly::PromiseAlreadySatisfied&) {
      return false;
    }
  }

  non_atomic bool is_fulfilled() {
    return promise.isFulfilled();
  }
};

target_method_dual(ltest::generators::genEmpty, int,
                   FollyCoroSharedPromiseTarget, get);

target_method(genPositiveInt, bool,
              FollyCoroSharedPromiseTarget, set_value, int);

target_method(ltest::generators::genEmpty, bool,
              FollyCoroSharedPromiseTarget, is_fulfilled);

using spec_t =
    ltest::SpecDual<FollyCoroSharedPromiseTarget,
                    spec::FollyCoroSharedPromise,
                    spec::FollyCoroSharedPromiseHash,
                    spec::FollyCoroSharedPromiseEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
