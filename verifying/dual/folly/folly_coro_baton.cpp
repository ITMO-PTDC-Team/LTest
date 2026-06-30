//
// Created by bitree.
//

#include "../../specs/folly/coro_baton.h"

#include "../../../runtime/include/verifying.h"

#include <folly/coro/Baton.h>

struct FollyCoroBatonTarget {
  folly::coro::Baton baton;

  non_atomic auto wait() {
    return baton.operator co_await();
  }

  non_atomic void post() {
    baton.post();
  }

  non_atomic void reset() {
    baton.reset();
  }

  non_atomic bool ready() {
    return baton.ready();
  }
};

target_method_dual(ltest::generators::genEmpty, void,
                   FollyCoroBatonTarget, wait);

target_method(ltest::generators::genEmpty, void,
              FollyCoroBatonTarget, post);

target_method(ltest::generators::genEmpty, void,
              FollyCoroBatonTarget, reset);

target_method(ltest::generators::genEmpty, bool,
              FollyCoroBatonTarget, ready);

using spec_t =
    ltest::SpecDual<FollyCoroBatonTarget,
                    spec::FollyCoroBaton,
                    spec::FollyCoroBatonHash,
                    spec::FollyCoroBatonEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
