#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

#include "scheduler.h"
#include "workload_policy.h"

// ----------------------------
// Default verifier: allows everything
// ----------------------------
struct DefaultStrategyTaskVerifier {
  inline void OnRoundStart(std::size_t /*threads*/) {}

  inline void OnTaskStarted(const std::string& /*method*/,
                            std::size_t /*thread_id*/,
                            int /*task_id*/) {}

  inline bool VerifyStart(const std::string& /*method*/,
                          std::size_t /*thread_id*/,
                          const ltest::StartContext& /*ctx*/) {
    return true;
  }

  inline bool Verify(const std::string& /*name*/, std::size_t /*thread_id*/) {
    return true;
  }

  inline void OnFinished(Task& /*task*/, std::size_t /*thread_id*/) {}

  inline std::optional<std::string> ReleaseTask(std::size_t /*thread_id*/) {
    return std::nullopt;
  }
};

// ----------------------------
// ReservePolicyVerifier: workload policy from spec (Reserve rules, Blocked-trigger MVP)
// Wraps a base verifier (protocol constraints) and adds workload veto.
// ----------------------------
template <class LinearSpec, class BaseVerifier = DefaultStrategyTaskVerifier>
struct ReservePolicyVerifier {
  inline void OnRoundStart(std::size_t threads) { base_.OnRoundStart(threads); }

  inline void OnTaskStarted(const std::string& method,
                            std::size_t thread_id,
                            int task_id) {
    base_.OnTaskStarted(method, thread_id, task_id);
  }

  inline bool VerifyStart(const std::string& method,
                          std::size_t thread_id,
                          const ltest::StartContext& ctx) {
    // First: protocol constraints (if any)
    if (!base_.VerifyStart(method, thread_id, ctx)) {
      return false;
    }

    // Then: reserve rules from spec.
    // If LinearSpec has no GetWorkloadPolicy(), compilation will fail.
    // We will only instantiate this verifier when GetWorkloadPolicy() exists (Step 6).
    ltest::WorkloadPolicy pol = LinearSpec::GetWorkloadPolicy();

    // Collect all triggered reserve rules (Blocked-trigger).
    // triggered iff there is at least one blocked task of wait_method.
    std::unordered_set<std::string> allowed;  // union of progress_methods
    std::size_t reserve_threads_needed = 0;
    bool any_triggered = false;

    for (const auto& r : pol.reserve) {
      auto it = ctx.active_by_method.find(r.wait_method);
      if (it == ctx.active_by_method.end() || it->second <= 0) {
        continue;
      }
      any_triggered = true;
      reserve_threads_needed = std::max(reserve_threads_needed, r.reserve_threads);
      for (const auto& pm : r.progress_methods) {
        allowed.insert(pm);
      }
    }

    if (!any_triggered) {
      return true;
    }

    // When we are running out of free threads, force progress methods.
    // Typical case: reserve_threads_needed == 1, ctx.free_threads == 1 (last free thread).
    if (ctx.free_threads <= reserve_threads_needed) {
      // If policy didn't specify any progress methods, do not block generation.
      if (allowed.empty()) return true;
      return allowed.contains(method);
    }

    return true;
  }

  inline bool Verify(const std::string& name, std::size_t thread_id) {
    return base_.Verify(name, thread_id);
  }

  inline void OnFinished(Task& task, std::size_t thread_id) {
    base_.OnFinished(task, thread_id);
  }

  inline std::optional<std::string> ReleaseTask(std::size_t thread_id) {
    return base_.ReleaseTask(thread_id);
  }

 private:
  BaseVerifier base_{};
};