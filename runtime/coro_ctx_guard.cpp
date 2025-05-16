#include "coro_ctx_guard.h"

/// True in coroutine contexts
/// required for
/// 1. incapsulating CoroYield calls, allowing
/// to call methods annotated with non_atomic in scheduler fiber
/// 2. incapsulating syscall hook

bool ltest_coro_ctx = 0;

ltest::CoroCtxGuard::CoroCtxGuard() { ltest_coro_ctx = true; }

ltest::CoroCtxGuard::~CoroCtxGuard() { ltest_coro_ctx = false; }
