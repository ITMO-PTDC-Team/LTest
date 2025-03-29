#include <folly/SharedMutex.h>

#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"
#include "verifying/blocking/verifiers/shared_mutex_verifier.h"
#include "verifying/specs/mutex.h"

using spec_t =
    ltest::Spec<folly::SharedMutex, spec::SharedLinearMutex,
                spec::SharedLinearMutexHash, spec::SharedLinearMutexEquals>;

LTEST_ENTRYPOINT_CONSTRAINT(spec_t, spec::SharedMutexVerifier);

target_method(ltest::generators::genEmpty, int, folly::SharedMutex, lock);

target_method(ltest::generators::genEmpty, int, folly::SharedMutex, unlock);

int (folly::SharedMutexImpl<false>::*lock_shared)() =
    &folly::SharedMutexImpl<false>::lock_shared;

const char *lock_shared_task_name = "lock_shared";
ltest::TargetMethod<int, folly::SharedMutex> lock_shared_ltest_method_cls{
    lock_shared_task_name, ltest::generators::genEmpty, lock_shared};

int (folly::SharedMutexImpl<false>::*unlock_shared)() =
    &folly::SharedMutexImpl<false>::unlock_shared;

const char *unlock_shared_task_name = "unlock_shared";
ltest::TargetMethod<int, folly::SharedMutex> unlock_shared_ltest_method_cls{
    unlock_shared_task_name, ltest::generators::genEmpty, unlock_shared};
