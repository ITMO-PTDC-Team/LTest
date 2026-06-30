//
// Created by bitree.
//

#include "../../specs/folly/coro_shared_mutex.h"
#include "folly_coro_await_adapter.h"
#include "verifiers/folly_coro_shared_mutex_verifier.h"

#include "../../../runtime/include/verifying.h"

#include <folly/Executor.h>
#include <folly/coro/SharedMutex.h>
#include <folly/executors/InlineExecutor.h>
#include <folly/io/async/Request.h>

struct FollyCoroSharedMutexTarget {
  folly::InlineExecutor ex;
  folly::coro::SharedMutex m;
  bool harness_exclusive{false};
  int harness_shared{0};

  FollyCoroSharedMutexTarget() {
    WarmRequestContext();
  }

  static as_atomic void WarmRequestContext() {
    (void)folly::RequestContext::get();
  }

  struct SetExclusiveOnResume {
    bool* harness_exclusive;

    as_atomic void operator()() const {
      *harness_exclusive = true;
    }
  };

  struct IncSharedOnResume {
    int* harness_shared;

    as_atomic void operator()() const {
      ++*harness_shared;
    }
  };

  non_atomic auto lock() {
    return MakeFollyVoidAwaiterAdapter(
        m.co_lock().viaIfAsync(folly::getKeepAliveToken(ex)),
        SetExclusiveOnResume{&harness_exclusive});
  }

  non_atomic auto lock_shared() {
    return MakeFollyVoidAwaiterAdapter(
        m.co_lock_shared().viaIfAsync(folly::getKeepAliveToken(ex)),
        IncSharedOnResume{&harness_shared});
  }

  non_atomic bool try_lock() {
    if (ltest_round_terminating) {
      return false;
    }
    bool ok = m.try_lock();
    if (ok) {
      harness_exclusive = true;
    }
    return ok;
  }

  non_atomic bool try_lock_shared() {
    if (ltest_round_terminating) {
      return false;
    }
    bool ok = m.try_lock_shared();
    if (ok) {
      ++harness_shared;
    }
    return ok;
  }

  non_atomic void unlock() {
    if (ltest_round_terminating) {
      return;
    }
    if (!harness_exclusive) {
      return;
    }
    harness_exclusive = false;
    m.unlock();
  }

  non_atomic void unlock_shared() {
    if (ltest_round_terminating) {
      return;
    }
    if (harness_shared <= 0) {
      return;
    }
    --harness_shared;
    m.unlock_shared();
  }
};

target_method_dual(ltest::generators::genEmpty, void,
                   FollyCoroSharedMutexTarget, lock);

target_method_dual(ltest::generators::genEmpty, void,
                   FollyCoroSharedMutexTarget, lock_shared);

target_method(ltest::generators::genEmpty, bool,
              FollyCoroSharedMutexTarget, try_lock);

target_method(ltest::generators::genEmpty, bool,
              FollyCoroSharedMutexTarget, try_lock_shared);

target_method(ltest::generators::genEmpty, void,
              FollyCoroSharedMutexTarget, unlock);

target_method(ltest::generators::genEmpty, void,
              FollyCoroSharedMutexTarget, unlock_shared);

using spec_t =
    ltest::SpecDual<FollyCoroSharedMutexTarget,
                    spec::FollyCoroSharedMutex,
                    spec::FollyCoroSharedMutexHash,
                    spec::FollyCoroSharedMutexEquals>;

LTEST_ENTRYPOINT_DUAL_CONSTRAINT(spec_t, FollyCoroSharedMutexVerifier);
