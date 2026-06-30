//
// Created by bitree.
//

#include "../../specs/folly/coro_unbounded_queue.h"
#include "folly_coro_await_adapter.h"

#include "../../../runtime/include/verifying.h"

#include <cstdlib>

#include <folly/Executor.h>
#include <folly/coro/UnboundedQueue.h>
#include <folly/io/async/Request.h>

extern "C" const char* __asan_default_options() {
  return "detect_leaks=0";
}

static auto genPositiveInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

struct FollyCoroUnboundedQueueTarget {
  FollyLtestExecutor ex;
  folly::coro::UnboundedQueue<int> q;
  static constexpr int kTerminateWakeValue = -1;

  FollyCoroUnboundedQueueTarget() {
    WarmRequestContext();
    WarmHazptrQueue();
  }

  static as_atomic void WarmRequestContext() {
    (void)folly::RequestContext::get();
  }

  as_atomic void WarmHazptrQueue() {
    q.enqueue(1);
    (void)q.try_dequeue();
  }

  non_atomic void enqueue(int value) {
    q.enqueue(value);
    ex.driveAll();
  }

  static folly::coro::Task<int> DequeueOwned(
      FollyCoroUnboundedQueueTarget* self) {
    int value = 0;
    co_await self->q.dequeue(value);
    co_return value;
  }

  struct WakeBlockedDequeue {
    FollyCoroUnboundedQueueTarget* self;

    as_atomic void operator()() const {
      self->q.enqueue(kTerminateWakeValue);
    }
  };

  non_atomic auto dequeue() {
    return MakeFollyTaskViaIfAsync<false, false>(
        DequeueOwned(this), ex, WakeBlockedDequeue{this});
  }

  non_atomic int try_dequeue() {
    auto value = q.try_dequeue();
    if (!value) {
      return spec::FollyCoroUnboundedQueue::kEmptyTryDequeue;
    }
    return *value;
  }
};

target_method(genPositiveInt, void,
              FollyCoroUnboundedQueueTarget, enqueue, int);

target_method_dual(ltest::generators::genEmpty, int,
                   FollyCoroUnboundedQueueTarget, dequeue);

target_method(ltest::generators::genEmpty, int,
              FollyCoroUnboundedQueueTarget, try_dequeue);

using spec_t =
    ltest::SpecDual<FollyCoroUnboundedQueueTarget,
                    spec::FollyCoroUnboundedQueue,
                    spec::FollyCoroUnboundedQueueHash,
                    spec::FollyCoroUnboundedQueueEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
