#include "yield_guard.h"

/// Required for incapsulating CoroYield calls only in coroutines code, allowing to call methods
/// annotated with non_atomic in scheduler fiber
bool __yield = 0;

ltest::YieldGuard::YieldGuard() { __yield = true; }

ltest::YieldGuard::~YieldGuard() { __yield = false; }
