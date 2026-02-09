#pragma once
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lincheck.h"       // Invoke/Response (ValueWrapper), ModelChecker
#include "value_wrapper.h"  // ValueWrapper

// -----------------------------
// Dual history events
// -----------------------------
struct RequestInvoke {
  explicit RequestInvoke(const Task& task, int thread_id)
      : task(task), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }
  int thread_id;

 private:
  std::reference_wrapper<const Task> task;
};

struct RequestResponse {
  explicit RequestResponse(const Task& task, int thread_id)
      : task(task), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }
  int thread_id;

 private:
  std::reference_wrapper<const Task> task;
};

struct FollowUpInvoke {
  explicit FollowUpInvoke(const Task& task, int thread_id)
      : task(task), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }
  int thread_id;

 private:
  std::reference_wrapper<const Task> task;
};

struct FollowUpResponse {
  FollowUpResponse(const Task& task, ValueWrapper result, int thread_id)
      : task(task), result(std::move(result)), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }

  ValueWrapper result;
  int thread_id;

 private:
  std::reference_wrapper<const Task> task;
};

// Mixed history: allows to combine ordinary Invoke/Response and dual events.
// (useful later for "size()" operations, etc.)
using DualHistoryEvent =
    std::variant<Invoke, Response, RequestInvoke, RequestResponse,
                 FollowUpInvoke, FollowUpResponse>;

struct DualModelChecker {
  virtual bool Check(const std::vector<DualHistoryEvent>& history) = 0;
  virtual ~DualModelChecker() = default;
};

// -----------------------------
// Dual spec method types
// -----------------------------
template <class SpecState>
using DualNonBlockingMethod =
    std::function<ValueWrapper(SpecState*, void* args)>;

template <class SpecState>
using DualRequestMethod =
    std::function<void(SpecState*, void* args, int op_id)>;

// If follow-up cannot be completed yet => std::nullopt (cannot linearize now)
template <class SpecState>
using DualFollowUpMethod = std::function<std::optional<ValueWrapper>(
    SpecState*, void* args, int op_id)>;

template <class SpecState>
using DualBlockingMethod =
    std::pair<DualRequestMethod<SpecState>, DualFollowUpMethod<SpecState>>;

template <class SpecState>
using DualMethod = std::variant<DualNonBlockingMethod<SpecState>,
                                DualBlockingMethod<SpecState>>;

template <class SpecState>
using DualMethodMap = std::map<std::string, DualMethod<SpecState>, std::less<>>;

// -----------------------------
// Helpers: event classification + mapping invoke->response
// -----------------------------
inline bool IsDualResponseEvent(const DualHistoryEvent& e) {
  return std::holds_alternative<Response>(e) ||
         std::holds_alternative<RequestResponse>(e) ||
         std::holds_alternative<FollowUpResponse>(e);
}

inline bool IsDualInvokeEvent(const DualHistoryEvent& e) {
  return std::holds_alternative<Invoke>(e) ||
         std::holds_alternative<RequestInvoke>(e) ||
         std::holds_alternative<FollowUpInvoke>(e);
}

inline const Task& GetTaskOfEvent(const DualHistoryEvent& e) {
  return std::visit([](auto const& ev) -> const Task& { return ev.GetTask(); },
                    e);
}

inline std::string GetNameOfEvent(const DualHistoryEvent& e) {
  return std::string(GetTaskOfEvent(e)->GetName());
}

inline void* GetArgsOfEvent(const DualHistoryEvent& e) {
  return GetTaskOfEvent(e)->GetArgs();
}

inline int GetOpIdOfEvent(const DualHistoryEvent& e) {
  return GetTaskOfEvent(e)->GetId();
}

// Build mapping invoke_index -> response_index for:
// Invoke/Response, RequestInvoke/RequestResponse,
// FollowUpInvoke/FollowUpResponse.
inline std::map<size_t, size_t> get_dual_inv_res_mapping(
    const std::vector<DualHistoryEvent>& history) {
  std::unordered_map<int, size_t> inv_idx;
  std::unordered_map<int, size_t> req_inv_idx;
  std::unordered_map<int, size_t> fol_inv_idx;

  std::map<size_t, size_t> inv_res;

  for (size_t i = 0; i < history.size(); ++i) {
    const auto& e = history[i];

    if (std::holds_alternative<Invoke>(e)) {
      inv_idx[GetOpIdOfEvent(e)] = i;
      continue;
    }
    if (std::holds_alternative<RequestInvoke>(e)) {
      req_inv_idx[GetOpIdOfEvent(e)] = i;
      continue;
    }
    if (std::holds_alternative<FollowUpInvoke>(e)) {
      fol_inv_idx[GetOpIdOfEvent(e)] = i;
      continue;
    }

    if (std::holds_alternative<Response>(e)) {
      int id = GetOpIdOfEvent(e);
      auto it = inv_idx.find(id);
      if (it != inv_idx.end()) inv_res[it->second] = i;
      continue;
    }
    if (std::holds_alternative<RequestResponse>(e)) {
      int id = GetOpIdOfEvent(e);
      auto it = req_inv_idx.find(id);
      if (it != req_inv_idx.end()) inv_res[it->second] = i;
      continue;
    }
    if (std::holds_alternative<FollowUpResponse>(e)) {
      int id = GetOpIdOfEvent(e);
      auto it = fol_inv_idx.find(id);
      if (it != fol_inv_idx.end()) inv_res[it->second] = i;
      continue;
    }
  }

  return inv_res;
}

// -----------------------------
// Dual checker (recursive, copyable spec state)
// -----------------------------
template <class SpecState>
struct LinearizabilityDualCheckerRecursive final : DualModelChecker {
  using MethodMap = DualMethodMap<SpecState>;

  LinearizabilityDualCheckerRecursive() = delete;

  LinearizabilityDualCheckerRecursive(MethodMap methods, SpecState init_state)
      : methods(std::move(methods)), init_state(std::move(init_state)) {
    if (!std::is_copy_constructible_v<SpecState>) {
      throw std::invalid_argument("SpecState must be copy constructible");
    }
  }

  bool Check(const std::vector<DualHistoryEvent>& history) override {
    if (history.empty()) return true;

    auto inv_res = get_dual_inv_res_mapping(history);

    std::vector<bool> linearized(history.size(), false);

    std::function<bool(SpecState, std::vector<bool>&)> step;

    step = [&](SpecState state, std::vector<bool>& lin) -> bool {
      // done?
      bool all = true;
      for (bool b : lin) all &= b;
      if (all) return true;

      // iterate minimal operations (like in recursive checker):
      for (size_t i = 0; i < history.size(); ++i) {
        if (lin[i]) continue;

        // once we see a not-linearized response, everything to the right
        // is not minimal in this simplified model
        if (IsDualResponseEvent(history[i])) break;

        // only invoke-type events are candidates
        if (!IsDualInvokeEvent(history[i])) continue;

        const auto& ev = history[i];
        const std::string name = GetNameOfEvent(ev);
        void* args = GetArgsOfEvent(ev);
        int op_id = GetOpIdOfEvent(ev);

        auto mit = methods.find(name);
        if (mit == methods.end()) {
          throw std::runtime_error("No method in dual spec for: " + name);
        }

        bool has_resp = inv_res.find(i) != inv_res.end();
        size_t resp_i = has_resp ? inv_res[i] : 0;

        // ---- Ordinary Invoke ----
        if (std::holds_alternative<Invoke>(ev)) {
          if (!std::holds_alternative<DualNonBlockingMethod<SpecState>>(
                  mit->second)) {
            // TODO(bitree): ordinary Invoke must map to nonblocking method.
            continue;
          }
          auto m = std::get<DualNonBlockingMethod<SpecState>>(mit->second);

          SpecState st2 = state;
          ValueWrapper expected = m(&st2, args);

          if (!has_resp) {
            lin[i] = true;
            if (step(st2, lin)) return true;
            lin[i] = false;
            continue;
          }

          const auto& resp = std::get<Response>(history[resp_i]);
          if (expected == resp.result) {
            lin[i] = true;
            lin[resp_i] = true;
            if (step(st2, lin)) return true;
            lin[i] = false;
            lin[resp_i] = false;
          }
          continue;
        }

        // ---- RequestInvoke ----
        if (std::holds_alternative<RequestInvoke>(ev)) {
          if (!std::holds_alternative<DualBlockingMethod<SpecState>>(
                  mit->second)) {
            continue;
          }
          auto [req, fol] =
              std::get<DualBlockingMethod<SpecState>>(mit->second);

          SpecState st2 = state;
          req(&st2, args, op_id);

          lin[i] = true;
          if (has_resp) lin[resp_i] = true;

          if (step(st2, lin)) return true;

          lin[i] = false;
          if (has_resp) lin[resp_i] = false;
          continue;
        }

        // ---- FollowUpInvoke ----
        if (std::holds_alternative<FollowUpInvoke>(ev)) {
          if (!std::holds_alternative<DualBlockingMethod<SpecState>>(
                  mit->second)) {
            continue;
          }
          auto [req, fol] =
              std::get<DualBlockingMethod<SpecState>>(mit->second);

          SpecState st2 = state;
          auto opt_expected = fol(&st2, args, op_id);
          if (!opt_expected.has_value()) {
            // not ready yet => cannot linearize now
            continue;
          }
          ValueWrapper expected = std::move(opt_expected.value());

          if (!has_resp) {
            lin[i] = true;
            if (step(st2, lin)) return true;
            lin[i] = false;
            continue;
          }

          const auto& resp = std::get<FollowUpResponse>(history[resp_i]);
          if (expected == resp.result) {
            lin[i] = true;
            lin[resp_i] = true;
            if (step(st2, lin)) return true;
            lin[i] = false;
            lin[resp_i] = false;
          }
          continue;
        }
      }

      return false;
    };

    return step(init_state, linearized);
  }

 private:
  MethodMap methods;
  SpecState init_state;
};
