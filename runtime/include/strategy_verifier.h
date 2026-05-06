#pragma once

#include <algorithm>
#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
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

  inline bool VerifyExisting(Task& task, std::size_t thread_id) {
    return Verify(std::string(task->GetName()), thread_id);
  }

  inline void OnFinished(Task& /*task*/, std::size_t /*thread_id*/) {}

  inline std::optional<std::string> ReleaseTask(std::size_t /*thread_id*/) {
    return std::nullopt;
  }

  inline std::vector<std::string> GetDeadlockProgressMethods(
      const std::string& /*method*/) const {
    return {};
  }
};

// ----------------------------
// ReservePolicyVerifier:
//  - семантические ограничения из BaseVerifier
//  - reserve rules
//  - prefix budget rules
//  - max_active / max_blocked rules
// ----------------------------
template <class LinearSpec, class BaseVerifier = DefaultStrategyTaskVerifier>
struct ReservePolicyVerifier {
  inline void OnRoundStart(std::size_t threads) {
    threads_ = threads;
    started_by_method_.clear();
    started_task_ids_.clear();
    base_.OnRoundStart(threads);
  }

  inline void OnTaskStarted(const std::string& method,
                            std::size_t thread_id,
                            int task_id) {
    if (!started_task_ids_.insert(task_id).second) {
      return;
    }
    base_.OnTaskStarted(method, thread_id, task_id);
    ++started_by_method_[method];
  }

  inline bool VerifyStart(const std::string& method,
                          std::size_t thread_id,
                          const ltest::StartContext& ctx) {
    // Сначала обычные семантические ограничения verifier'а.
    if (!base_.VerifyStart(method, thread_id, ctx)) {
      return false;
    }

    ltest::WorkloadPolicy pol = LinearSpec::GetWorkloadPolicy();

    auto started_if = [&](const std::string& name) -> std::size_t {
      std::size_t val = 0;
      auto it = started_by_method_.find(name);
      if (it != started_by_method_.end()) {
        val = it->second;
      }
      if (name == method) {
        ++val;
      }
      return val;
    };

    auto active_if = [&](const std::string& name) -> std::size_t {
      std::size_t val = 0;
      auto it = ctx.active_by_method.find(name);
      if (it != ctx.active_by_method.end()) {
        val = static_cast<std::size_t>(it->second);
      }
      if (name == method) {
        ++val;
      }
      return val;
    };

    auto blocked_now = [&](const std::string& name) -> std::size_t {
      auto it = ctx.blocked_by_method.find(name);
      if (it == ctx.blocked_by_method.end()) {
        return 0;
      }
      return static_cast<std::size_t>(it->second);
    };

    // 1. Ограничение на число активных задач метода
    for (const auto& r : pol.max_active) {
      if (r.method == method && active_if(method) > r.max_active) {
        return false;
      }
    }

    // 2. Ограничение на число уже заблокированных задач метода
    for (const auto& r : pol.max_blocked) {
      if (r.method == method && blocked_now(method) >= r.max_blocked) {
        return false;
      }
    }

    // 3. Префиксный "бюджет прогресса"
    for (const auto& r : pol.prefix_budget) {
      const std::size_t lhs = started_if(r.wait_method);

      std::size_t rhs = r.initial_credit + ctx.threads - r.reserve_threads;
      for (const auto& pm : r.progress_methods) {
        rhs += started_if(pm);
      }

      if (lhs > rhs) {
        return false;
      }
    }

    // 4. blocked-trigger reserve rules
    std::unordered_set<std::string> allowed;
    std::size_t reserve_threads_needed = 0;
    bool any_triggered = false;

    for (const auto& r : pol.reserve) {
      auto blocked = blocked_now(r.wait_method);
      if (blocked == 0) {
        continue;
      }

      any_triggered = true;
      reserve_threads_needed =
          std::max(reserve_threads_needed, r.reserve_threads);

      for (const auto& pm : r.progress_methods) {
        allowed.insert(pm);
      }
    }

    if (!any_triggered) {
      return true;
    }

    // Если свободных потоков уже мало, разрешаем стартовать только методы прогресса.
    if (ctx.free_threads <= reserve_threads_needed) {
      if (allowed.empty()) {
        return true;
      }
      return allowed.contains(method);
    }

    return true;
  }

  inline bool Verify(const std::string& name, std::size_t thread_id) {
    return base_.Verify(name, thread_id);
  }

  inline bool VerifyExisting(Task& task, std::size_t thread_id) {
    if constexpr (requires(BaseVerifier& base, Task& existing_task,
                           std::size_t existing_thread_id) {
                    base.VerifyExisting(existing_task, existing_thread_id);
                  }) {
      return base_.VerifyExisting(task, thread_id);
    } else {
      return base_.Verify(std::string(task->GetName()), thread_id);
    }
  }

  inline void OnFinished(Task& task, std::size_t thread_id) {
    base_.OnFinished(task, thread_id);
  }

  inline std::optional<std::string> ReleaseTask(std::size_t thread_id) {
    return base_.ReleaseTask(thread_id);
  }

  inline std::vector<std::string> GetDeadlockProgressMethods(
      const std::string& method) const {
    auto add_unique = [](std::vector<std::string>& out,
                         const std::vector<std::string>& methods) {
      for (const auto& name : methods) {
        if (std::find(out.begin(), out.end(), name) == out.end()) {
          out.push_back(name);
        }
      }
    };

    ltest::WorkloadPolicy pol = LinearSpec::GetWorkloadPolicy();

    std::vector<std::string> from_explicit_rules;
    for (const auto& r : pol.rollback) {
      if (r.wait_method == method) {
        add_unique(from_explicit_rules, r.progress_methods);
      }
    }
    if (!from_explicit_rules.empty()) {
      return from_explicit_rules;
    }

    std::vector<std::string> fallback;
    for (const auto& r : pol.reserve) {
      if (r.wait_method == method) {
        add_unique(fallback, r.progress_methods);
      }
    }
    for (const auto& r : pol.prefix_budget) {
      if (r.wait_method == method) {
        add_unique(fallback, r.progress_methods);
      }
    }
    return fallback;
  }

 private:
  BaseVerifier base_{};
  std::unordered_map<std::string, std::size_t> started_by_method_;
  std::unordered_set<int> started_task_ids_;
  std::size_t threads_{0};
};
