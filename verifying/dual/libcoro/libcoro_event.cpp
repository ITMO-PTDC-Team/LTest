#include "../../specs/libcoro/event.h"

#include "../../../runtime/include/verifying.h"

#include <coro/event.hpp>

struct LibcoroEventTarget {
  coro::event ev;

  non_atomic auto wait() {
    return ev.operator co_await();
  }

  non_atomic void set() {
    ev.set();
  }

  non_atomic void reset() {
    ev.reset();
  }

  non_atomic bool is_set() {
    return ev.is_set();
  }
};

target_method_dual(ltest::generators::genEmpty, void,
                   LibcoroEventTarget, wait);
target_method(ltest::generators::genEmpty, void,
              LibcoroEventTarget, set);
target_method(ltest::generators::genEmpty, void,
              LibcoroEventTarget, reset);
target_method(ltest::generators::genEmpty, bool,
              LibcoroEventTarget, is_set);

using spec_t =
    ltest::SpecDual<LibcoroEventTarget,
                    spec::LibcoroEvent,
                    spec::LibcoroEventHash,
                    spec::LibcoroEventEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
