#pragma once

extern bool __yield;

namespace ltest {

struct YieldGuard {
  YieldGuard();
  ~YieldGuard();
};

}  // namespace ltest