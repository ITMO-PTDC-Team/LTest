#include "yield_guard.h"

/// Required for incapsulating CoroYield calls only in coroutines code, allowing
/// to call methods annotated with non_atomic in scheduler fiber
bool ltest_yield = 0;

ltest::AllowYieldArea::AllowYieldArea() { ltest_yield = true; }

ltest::AllowYieldArea::~AllowYieldArea() { ltest_yield = false; }
