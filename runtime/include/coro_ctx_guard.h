#pragma once

namespace ltest {

// These flags follow the logical TLS when a fiber switches contexts.
extern thread_local bool ltest_coro_ctx;
extern thread_local bool ltest_target_construction;

struct CoroCtxGuard {
  bool previous;
  CoroCtxGuard();
  ~CoroCtxGuard();
};

struct SchedCtxGuard {
  bool tmp;
  SchedCtxGuard();
  ~SchedCtxGuard();
};

struct TargetConstructionGuard {
  bool previous;
  TargetConstructionGuard();
  ~TargetConstructionGuard();
};

}  // namespace ltest
