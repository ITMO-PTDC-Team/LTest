#pragma once
#include <concepts>
#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lincheck.h"       // Invoke/Response (ValueWrapper), ModelChecker
#include "linearization_search.h"
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
  Task task;
};

struct RequestResponse {
  explicit RequestResponse(const Task& task, int thread_id)
      : task(task), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }
  int thread_id;

 private:
  Task task;
};

struct FollowUpInvoke {
  explicit FollowUpInvoke(const Task& task, int thread_id)
      : task(task), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }
  int thread_id;

 private:
  Task task;
};

struct FollowUpResponse {
  FollowUpResponse(const Task& task, ValueWrapper result, int thread_id)
      : task(task), result(std::move(result)), thread_id(thread_id) {}
  [[nodiscard]] const Task& GetTask() const { return task; }

  ValueWrapper result;
  int thread_id;

 private:
  Task task;
};

// Mixed history: allows to combine ordinary Invoke/Response and dual events.
// (useful later for "size()" operations, etc.)
using DualHistoryEvent =
    std::variant<Invoke, Response, RequestInvoke, RequestResponse,
                 FollowUpInvoke, FollowUpResponse>;

static_assert(std::variant_size_v<DualHistoryEvent> == 6);
static_assert(std::is_same_v<std::variant_alternative_t<0, DualHistoryEvent>,
                             Invoke>);
static_assert(std::is_same_v<std::variant_alternative_t<1, DualHistoryEvent>,
                             Response>);
static_assert(std::is_same_v<std::variant_alternative_t<2, DualHistoryEvent>,
                             RequestInvoke>);
static_assert(std::is_same_v<std::variant_alternative_t<3, DualHistoryEvent>,
                             RequestResponse>);
static_assert(std::is_same_v<std::variant_alternative_t<4, DualHistoryEvent>,
                             FollowUpInvoke>);
static_assert(std::is_same_v<std::variant_alternative_t<5, DualHistoryEvent>,
                             FollowUpResponse>);

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

// Predicate-style ordinary operation. Use when the target API can legitimately
// return more than one value for the same abstract state, for example
// try-lock APIs that may fail without changing state.
template <class SpecState>
using DualNonBlockingPredicateMethod =
    std::function<bool(SpecState*, void* args, const ValueWrapper* result)>;

template <class SpecState>
using DualRequestMethod =
    std::function<void(SpecState*, void* args, int op_id)>;

// If follow-up cannot be completed yet => std::nullopt (cannot linearize now)
template <class SpecState>
using DualFollowUpMethod = std::function<std::optional<ValueWrapper>(
    SpecState*, void* args, int op_id)>;

template <class SpecState>
struct DualBlockingMethod {
  DualRequestMethod<SpecState> request;
  DualFollowUpMethod<SpecState> followup;

  DualBlockingMethod() = default;

  DualBlockingMethod(DualRequestMethod<SpecState> req,
                     DualFollowUpMethod<SpecState> fol)
      : request(std::move(req)), followup(std::move(fol)) {}
};

template <class SpecState>
using DualMethod = std::variant<DualNonBlockingMethod<SpecState>,
                                DualNonBlockingPredicateMethod<SpecState>,
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

enum class DualHistoryEventKind : std::uint8_t {
  OrdinaryInvoke,
  OrdinaryResponse,
  RequestInvoke,
  RequestResponse,
  FollowUpInvoke,
  FollowUpResponse,
};

inline constexpr size_t kNoDualResponse = ltest::detail::kNoResponseIndex;

struct DualHistoryEventInfo {
  DualHistoryEventKind kind{};
  std::string_view name{};
  const CoroBase* task{};
  void* args{};
  int op_id{};
  size_t response_index{kNoDualResponse};
  const ValueWrapper* result{};

  [[nodiscard]] bool IsInvoke() const {
    return kind == DualHistoryEventKind::OrdinaryInvoke ||
           kind == DualHistoryEventKind::RequestInvoke ||
           kind == DualHistoryEventKind::FollowUpInvoke;
  }

  [[nodiscard]] bool IsResponse() const {
    return kind == DualHistoryEventKind::OrdinaryResponse ||
           kind == DualHistoryEventKind::RequestResponse ||
           kind == DualHistoryEventKind::FollowUpResponse;
  }
};

inline std::vector<DualHistoryEventInfo> BuildDualHistoryEventInfo(
    const std::vector<DualHistoryEvent>& history) {
  std::vector<DualHistoryEventInfo> info(history.size());
  std::unordered_map<const CoroBase*, size_t> inv_idx;
  std::unordered_map<const CoroBase*, size_t> req_inv_idx;
  std::unordered_map<const CoroBase*, size_t> fol_inv_idx;
  std::unordered_map<int, size_t> inv_idx_by_id;
  std::unordered_map<int, size_t> req_inv_idx_by_id;
  std::unordered_map<int, size_t> fol_inv_idx_by_id;

  inv_idx.reserve(history.size());
  req_inv_idx.reserve(history.size());
  fol_inv_idx.reserve(history.size());
  inv_idx_by_id.reserve(history.size());
  req_inv_idx_by_id.reserve(history.size());
  fol_inv_idx_by_id.reserve(history.size());

  auto fill_task_info = [](DualHistoryEventInfo& dst, const Task& task) {
    dst.name = task->GetName();
    dst.task = task.get();
    dst.args = task->GetArgs();
    dst.op_id = task->GetId();
  };

  for (size_t i = 0; i < history.size(); ++i) {
    const auto& e = history[i];
    auto& dst = info[i];

    switch (e.index()) {
      case 0: {
        const auto& ev = std::get<Invoke>(e);
        dst.kind = DualHistoryEventKind::OrdinaryInvoke;
        fill_task_info(dst, ev.GetTask());
        inv_idx[ev.GetTask().get()] = i;
        inv_idx_by_id[dst.op_id] = i;
        break;
      }
      case 1: {
        const auto& ev = std::get<Response>(e);
        dst.kind = DualHistoryEventKind::OrdinaryResponse;
        dst.result = &ev.result;
        fill_task_info(dst, ev.GetTask());
        auto it = inv_idx.find(ev.GetTask().get());
        if (it != inv_idx.end()) {
          info[it->second].response_index = i;
        } else {
          auto by_id = inv_idx_by_id.find(dst.op_id);
          if (by_id != inv_idx_by_id.end()) {
            info[by_id->second].response_index = i;
          }
        }
        break;
      }
      case 2: {
        const auto& ev = std::get<RequestInvoke>(e);
        dst.kind = DualHistoryEventKind::RequestInvoke;
        fill_task_info(dst, ev.GetTask());
        req_inv_idx[ev.GetTask().get()] = i;
        req_inv_idx_by_id[dst.op_id] = i;
        break;
      }
      case 3: {
        const auto& ev = std::get<RequestResponse>(e);
        dst.kind = DualHistoryEventKind::RequestResponse;
        fill_task_info(dst, ev.GetTask());
        auto it = req_inv_idx.find(ev.GetTask().get());
        if (it != req_inv_idx.end()) {
          info[it->second].response_index = i;
        } else {
          auto by_id = req_inv_idx_by_id.find(dst.op_id);
          if (by_id != req_inv_idx_by_id.end()) {
            info[by_id->second].response_index = i;
          }
        }
        break;
      }
      case 4: {
        const auto& ev = std::get<FollowUpInvoke>(e);
        dst.kind = DualHistoryEventKind::FollowUpInvoke;
        fill_task_info(dst, ev.GetTask());
        fol_inv_idx[ev.GetTask().get()] = i;
        fol_inv_idx_by_id[dst.op_id] = i;
        break;
      }
      case 5: {
        const auto& ev = std::get<FollowUpResponse>(e);
        dst.kind = DualHistoryEventKind::FollowUpResponse;
        dst.result = &ev.result;
        fill_task_info(dst, ev.GetTask());
        auto it = fol_inv_idx.find(ev.GetTask().get());
        if (it != fol_inv_idx.end()) {
          info[it->second].response_index = i;
        } else {
          auto by_id = fol_inv_idx_by_id.find(dst.op_id);
          if (by_id != fol_inv_idx_by_id.end()) {
            info[by_id->second].response_index = i;
          }
        }
        break;
      }
      default:
        throw std::logic_error("unexpected dual history event kind");
    }
  }

  return info;
}

// Build mapping invoke_index -> response_index for:
// Invoke/Response, RequestInvoke/RequestResponse,
// FollowUpInvoke/FollowUpResponse.
inline std::map<size_t, size_t> get_dual_inv_res_mapping(
    const std::vector<DualHistoryEvent>& history) {
  auto info = BuildDualHistoryEventInfo(history);
  std::map<size_t, size_t> inv_res;
  for (size_t i = 0; i < info.size(); ++i) {
    if (info[i].IsInvoke() && info[i].response_index != kNoDualResponse) {
      inv_res.emplace(i, info[i].response_index);
    }
  }
  return inv_res;
}

// -----------------------------
// Dual checker (recursive, copyable spec state)
// -----------------------------
template <class SpecState, class SpecStateHash = std::hash<SpecState>,
          class SpecStateEqual = std::equal_to<SpecState>>
struct LinearizabilityDualCheckerRecursive final : DualModelChecker {
  using MethodMap = DualMethodMap<SpecState>;

  struct PendingFollowUp {
    std::string name;
    const CoroBase* task{};
    void* args{};
    int op_id{};
  };

  LinearizabilityDualCheckerRecursive() = delete;

  LinearizabilityDualCheckerRecursive(MethodMap methods, SpecState init_state)
      : methods(std::move(methods)), init_state(std::move(init_state)) {
    if (!std::is_copy_constructible_v<SpecState>) {
      throw std::invalid_argument("SpecState must be copy constructible");
    }
  }

  bool Check(const std::vector<DualHistoryEvent>& history) override {
    if (history.empty()) return true;

    const auto event_info = BuildDualHistoryEventInfo(history);
    std::vector<const DualMethod<SpecState>*> method_by_event(history.size(),
                                                             nullptr);
    for (size_t i = 0; i < event_info.size(); ++i) {
      if (!event_info[i].IsInvoke()) continue;
      auto mit = methods.find(event_info[i].name);
      if (mit == methods.end()) {
        throw std::runtime_error("No method in dual spec for: " +
                                 std::string(event_info[i].name));
      }
      method_by_event[i] = &mit->second;
    }

    // A completed request without FollowUpResponse remains a pending operation
    // in the checked history. It may be completed after the observed prefix,
    // just like an ordinary Invoke without Response.
    std::unordered_set<const CoroBase*> completed_followups;
    completed_followups.reserve(history.size());
    for (const auto& event : event_info) {
      if (event.kind == DualHistoryEventKind::FollowUpResponse) {
        completed_followups.insert(event.task);
      }
    }

    std::vector<PendingFollowUp> incomplete_followups;
    incomplete_followups.reserve(history.size());
    for (const auto& event : event_info) {
      if (event.kind != DualHistoryEventKind::RequestInvoke ||
          event.response_index == kNoDualResponse ||
          completed_followups.contains(event.task)) {
        continue;
      }
      incomplete_followups.push_back(PendingFollowUp{
          .name = std::string(event.name),
          .task = event.task,
          .args = event.args,
          .op_id = event.op_id,
      });
    }

    struct Adapter {
      const std::vector<DualHistoryEventInfo>& events;
      const std::vector<const DualMethod<SpecState>*>& methods;

      [[nodiscard]] bool IsInvoke(size_t index) const {
        return events[index].IsInvoke();
      }

      [[nodiscard]] bool IsResponse(size_t index) const {
        return events[index].IsResponse();
      }

      [[nodiscard]] size_t ResponseIndex(size_t index) const {
        return events[index].response_index;
      }

      [[nodiscard]] bool CanDropPendingInvoke(size_t index) const {
        const auto& event = events[index];
        if (event.response_index != kNoDualResponse) {
          return false;
        }

        return event.kind == DualHistoryEventKind::OrdinaryInvoke ||
               event.kind == DualHistoryEventKind::RequestInvoke ||
               event.kind == DualHistoryEventKind::FollowUpInvoke;
      }

      bool TryApply(size_t index, SpecState& state) const {
        const auto& event = events[index];
        const auto& method = *methods[index];
        const bool has_response = event.response_index != kNoDualResponse;

        if (event.kind == DualHistoryEventKind::OrdinaryInvoke) {
          if (std::holds_alternative<DualNonBlockingMethod<SpecState>>(
                  method)) {
            const auto& ordinary =
                std::get<DualNonBlockingMethod<SpecState>>(method);
            ValueWrapper expected = ordinary(&state, event.args);
            return !has_response ||
                   expected == *events[event.response_index].result;
          }

          if (std::holds_alternative<
                  DualNonBlockingPredicateMethod<SpecState>>(method)) {
            const auto& ordinary =
                std::get<DualNonBlockingPredicateMethod<SpecState>>(method);
            const ValueWrapper* result =
                has_response ? events[event.response_index].result : nullptr;
            return ordinary(&state, event.args, result);
          }

          return false;
        }

        if (event.kind == DualHistoryEventKind::RequestInvoke) {
          if (!std::holds_alternative<DualBlockingMethod<SpecState>>(method)) {
            return false;
          }

          const auto& blocking =
              std::get<DualBlockingMethod<SpecState>>(method);
          blocking.request(&state, event.args, event.op_id);
          return true;
        }

        if (event.kind == DualHistoryEventKind::FollowUpInvoke) {
          if (!std::holds_alternative<DualBlockingMethod<SpecState>>(method)) {
            return false;
          }

          const auto& blocking =
              std::get<DualBlockingMethod<SpecState>>(method);
          auto expected = blocking.followup(&state, event.args, event.op_id);
          if (!expected.has_value()) return false;
          return !has_response ||
                 expected.value() == *events[event.response_index].result;
        }

        return false;
      }
    };

    auto validate_completed_linearization =
        [this, &incomplete_followups](
            [[maybe_unused]] const SpecState& state) -> bool {
      for (const auto& pending : incomplete_followups) {
        auto mit = methods.find(pending.name);
        if (mit == methods.end() ||
            !std::holds_alternative<DualBlockingMethod<SpecState>>(
                mit->second)) {
          return false;
        }

      }

      return true;
    };

    return ltest::detail::RunRecursiveLinearizationSearchWithCache<
        SpecStateHash, SpecStateEqual>(
        history.size(), init_state, Adapter{event_info, method_by_event},
        validate_completed_linearization);
  }

 private:
  MethodMap methods;
  SpecState init_state;
};
