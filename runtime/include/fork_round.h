#pragma once

#include <functional>

#include "attempt.h"

namespace ltest {
// One worker, one initialized target, one attempt. The request is inherited by
// fork; the memfd returns the encoded report after blocking waitpid.
AttemptReport RunForkAttempt(const AttemptRequest& request, unsigned c_rand_seed,
                             const std::function<void()>& run);

}  // namespace ltest
