#pragma once

extern bool ltest_coro_ctx;

namespace ltest {

struct CoroCtxGuard {
  CoroCtxGuard();
  ~CoroCtxGuard();
};

struct SchedCtxGuard {
  bool tmp;
  SchedCtxGuard();
  ~SchedCtxGuard();
};

}  // namespace ltest