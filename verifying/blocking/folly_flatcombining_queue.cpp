#include <folly/synchronization/FlatCombining.h>

#include "runtime/include/generators.h"
#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"
#include "verifying/specs/queue.h"

class FlatCombiningQueue
    : public folly::FlatCombining<FlatCombiningQueue, ltest::mutex> {
  spec::Queue<> queue_;

 public:
  non_atomic void Push(int v) {
    this->requestFC([&]() { queue_.Push(v); });
    debug(stderr, "Push %d completed\n", v);
  }
  non_atomic int Pop() {
    int result;
    this->requestFC([&]() { result = queue_.Pop(); });
    debug(stderr, "Pop completed\n");
    return result;
  }
};

auto generateInt(size_t thread_num) {
  return std::make_tuple<int>(rand() % 10 + 1);
}

using spec_t = ltest::Spec<FlatCombiningQueue, spec::Queue<>, spec::QueueHash,
                           spec::QueueEquals>;

LTEST_ENTRYPOINT(spec_t);

target_method(generateInt, void, FlatCombiningQueue, Push, int);
target_method(ltest::generators::genEmpty, int, FlatCombiningQueue, Pop);
