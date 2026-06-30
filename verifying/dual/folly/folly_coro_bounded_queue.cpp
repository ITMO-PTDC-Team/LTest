//
// Created by bitree.
//

#include "../../specs/folly/coro_bounded_queue.h"
#include "folly_coro_await_adapter.h"

#include "../../../runtime/include/verifying.h"

#include <cstdlib>

#include <folly/Executor.h>
#include <folly/coro/BoundedQueue.h>
#include <folly/io/async/Request.h>

extern "C" const char* __asan_default_options() {
  return "detect_leaks=0";
}

static auto genPositiveInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

struct FollyCoroBoundedQueueTarget {
  FollyLtestExecutor ex;
  folly::coro::BoundedQueue<int> q{spec::FollyCoroBoundedQueue::kCapacity};

  FollyCoroBoundedQueueTarget() {
    WarmRequestContext();
  }

  static as_atomic void WarmRequestContext() {
    (void)folly::RequestContext::get();
  }

  static folly::coro::Task<void> EnqueueOwned(FollyCoroBoundedQueueTarget* self,
                                              int value) {
    co_await self->q.enqueue(value);
  }

  non_atomic auto enqueue(int value) {
    return MakeFollyTaskViaIfAsync(EnqueueOwned(this, value), ex);
  }

  non_atomic bool try_enqueue(int value) {
    bool ok = q.try_enqueue(value);
    ex.driveAll();
    return ok;
  }

  non_atomic auto dequeue() {
    return MakeFollyTaskViaIfAsync(q.dequeue(), ex);
  }

  non_atomic int try_dequeue() {
    auto value = q.try_dequeue();
    ex.driveAll();
    if (!value) {
      return spec::FollyCoroBoundedQueue::kEmptyTryDequeue;
    }
    return *value;
  }
};

target_method_dual(genPositiveInt, void,
                   FollyCoroBoundedQueueTarget, enqueue, int);

target_method(genPositiveInt, bool,
              FollyCoroBoundedQueueTarget, try_enqueue, int);

target_method_dual(ltest::generators::genEmpty, int,
                   FollyCoroBoundedQueueTarget, dequeue);

target_method(ltest::generators::genEmpty, int,
              FollyCoroBoundedQueueTarget, try_dequeue);

using spec_t =
    ltest::SpecDual<FollyCoroBoundedQueueTarget,
                    spec::FollyCoroBoundedQueue,
                    spec::FollyCoroBoundedQueueHash,
                    spec::FollyCoroBoundedQueueEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
