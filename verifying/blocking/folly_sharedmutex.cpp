#include <folly/SharedMutex.h>

#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"
#include "verifying/blocking/verifiers/shared_mutex_verifier.h"
#include "verifying/specs/mutex.h"

target_method(ltest::generators::genEmpty, int, folly::SharedMutex, lock);
// target_method(ltest::generators::genEmpty, int, folly::SharedMutex,
//               lock_shared);

target_method(ltest::generators::genEmpty, int, folly::SharedMutex, unlock);
// target_method(ltest::generators::genEmpty, int, folly::SharedMutex,
//               unlock_shared);

using spec_t =
    ltest::Spec<folly::SharedMutex, spec::SharedLinearMutex,
                spec::SharedLinearMutexHash, spec::SharedLinearMutexEquals>;

LTEST_ENTRYPOINT_CONSTRAINT(spec_t, spec::SharedMutexVerifier);
