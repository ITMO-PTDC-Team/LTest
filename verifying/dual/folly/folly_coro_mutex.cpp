//
// Created by bitree.
//

#include "../../specs/folly/coro_mutex.h"
#include "folly_coro_await_adapter.h"
#include "verifiers/folly_coro_mutex_verifier.h"

#include "../../../runtime/include/verifying.h"

#include <folly/Executor.h>
#include <folly/coro/Mutex.h>
#include <folly/executors/InlineExecutor.h>
#include <folly/io/async/Request.h>
#include <utility>

struct FollyCoroMutexTarget {
  folly::InlineExecutor ex;
  folly::coro::Mutex m;
  bool harness_locked{false};

  FollyCoroMutexTarget() {
    WarmRequestContext();
  }

  static as_atomic void WarmRequestContext() {
    // viaIfAsync saves folly::RequestContext in await_suspend(). Initializing
    // the singleton inside an LTEST fiber can block on Folly's internal mutex.
    (void)folly::RequestContext::get();
  }

  struct SetLockedOnResume {
    bool* harness_locked;

    as_atomic void operator()() const {
      *harness_locked = true;
    }
  };

  non_atomic auto lock() {
    return MakeFollyVoidAwaiterAdapter(
        m.co_lock().viaIfAsync(folly::getKeepAliveToken(ex)),
        SetLockedOnResume{&harness_locked});
  }

  non_atomic bool try_lock() {
    if (ltest_round_terminating) {
      return false;
    }
    bool ok = m.try_lock();
    if (ok) {
      harness_locked = true;
    }
    return ok;
  }

  non_atomic void unlock() {
    if (ltest_round_terminating) {
      return;
    }
    // Cleanup may replay an owner release after the wrapped Folly mutex has
    // already become free. Folly treats that as UB, so keep the harness guard
    // outside the mutex itself.
    if (!harness_locked) {
      return;
    }
    harness_locked = false;
    m.unlock();
  }
};

target_method_dual(ltest::generators::genEmpty, void,
                   FollyCoroMutexTarget, lock);

target_method(ltest::generators::genEmpty, bool,
              FollyCoroMutexTarget, try_lock);

target_method(ltest::generators::genEmpty, void,
              FollyCoroMutexTarget, unlock);

using spec_t =
    ltest::SpecDual<FollyCoroMutexTarget,
                    spec::FollyCoroMutex,
                    spec::FollyCoroMutexHash,
                    spec::FollyCoroMutexEquals>;

LTEST_ENTRYPOINT_DUAL_CONSTRAINT(spec_t, FollyCoroMutexVerifier);
