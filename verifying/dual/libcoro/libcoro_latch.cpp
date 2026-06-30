#include "../../specs/libcoro/latch.h"

#include "../../../runtime/include/verifying.h"

#include <coro/latch.hpp>
#include <cstdlib>

static auto genCountDown(size_t) {
  return ltest::generators::makeSingleArg<std::int64_t>(rand() % 2 + 1);
}

struct LibcoroLatchTarget : coro::latch {
  LibcoroLatchTarget() : coro::latch(spec::LibcoroLatch::kInitial) {}

  non_atomic auto wait() {
    return this->operator co_await();
  }

  non_atomic void count_down(std::int64_t count) {
    coro::latch::count_down(count);
  }

  non_atomic std::size_t remaining() const {
    return coro::latch::remaining();
  }

  non_atomic bool is_ready() const {
    return coro::latch::is_ready();
  }
};

target_method_dual(ltest::generators::genEmpty, void,
                   LibcoroLatchTarget, wait);
target_method(genCountDown, void,
              LibcoroLatchTarget, count_down, std::int64_t);
target_method(ltest::generators::genEmpty, std::size_t,
              LibcoroLatchTarget, remaining);
target_method(ltest::generators::genEmpty, bool,
              LibcoroLatchTarget, is_ready);

using spec_t =
    ltest::SpecDual<LibcoroLatchTarget,
                    spec::LibcoroLatch,
                    spec::LibcoroLatchHash,
                    spec::LibcoroLatchEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
