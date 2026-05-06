#pragma once

#include <functional>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include "lincheck.h"
#include "linearization_search.h"

namespace ltest::detail {

struct RecursiveHistoryEventInfo {
  bool is_invoke{};
  bool is_response{};
  std::string_view name{};
  void* args{};
  size_t response_index{kNoResponseIndex};
  const ValueWrapper* result{};
};

inline std::vector<RecursiveHistoryEventInfo> BuildRecursiveHistoryEventInfo(
    const std::vector<HistoryEvent>& history) {
  std::vector<RecursiveHistoryEventInfo> info(history.size());
  std::unordered_map<const CoroBase*, size_t> invoke_by_task;
  std::unordered_map<int, size_t> invoke_by_op_id;
  invoke_by_task.reserve(history.size());
  invoke_by_op_id.reserve(history.size());

  for (size_t i = 0; i < history.size(); ++i) {
    auto& dst = info[i];
    if (history[i].index() == 0) {
      const auto& invoke = std::get<Invoke>(history[i]);
      const auto& task = invoke.GetTask();
      dst.is_invoke = true;
      dst.name = task->GetName();
      dst.args = task->GetArgs();
      invoke_by_task[task.get()] = i;
      invoke_by_op_id[task->GetId()] = i;
      continue;
    }

    const auto& response = std::get<Response>(history[i]);
    const auto& task = response.GetTask();
    dst.is_response = true;
    dst.name = task->GetName();
    dst.args = task->GetArgs();
    dst.result = &response.result;

    auto invoke_it = invoke_by_task.find(task.get());
    if (invoke_it != invoke_by_task.end()) {
      info[invoke_it->second].response_index = i;
    } else {
      auto by_id = invoke_by_op_id.find(task->GetId());
      if (by_id != invoke_by_op_id.end()) {
        info[by_id->second].response_index = i;
      }
    }
  }

  return info;
}

}  // namespace ltest::detail

// Recursive checker adapter over the shared backtracking search.
template <class LinearSpecificationObject,
          class SpecificationObjectHash = std::hash<LinearSpecificationObject>,
          class SpecificationObjectEqual =
              std::equal_to<LinearSpecificationObject>>
struct LinearizabilityCheckerRecursive : ModelChecker {
  using Method = std::function<ValueWrapper(LinearSpecificationObject*, void*)>;
  using MethodMap = std::map<MethodName, Method>;

  LinearizabilityCheckerRecursive() = delete;

  LinearizabilityCheckerRecursive(MethodMap specification_methods,
                                  LinearSpecificationObject first_state);

  bool Check(const std::vector<HistoryEvent>& fixed_history) override;

 private:
  MethodMap specification_methods;
  LinearSpecificationObject first_state;
};

template <class LinearSpecificationObject, class SpecificationObjectHash,
          class SpecificationObjectEqual>
LinearizabilityCheckerRecursive<LinearSpecificationObject,
                                SpecificationObjectHash,
                                SpecificationObjectEqual>::
    LinearizabilityCheckerRecursive(
        LinearizabilityCheckerRecursive::MethodMap specification_methods,
        LinearSpecificationObject first_state)
    : specification_methods(specification_methods), first_state(first_state) {
  if (!std::is_copy_constructible_v<LinearSpecificationObject>) {
    // TODO: should do it in the compile time
    throw std::invalid_argument(
        "LinearSpecificationObject type have to be copy constructible");
  }
}

template <class LinearSpecificationObject, class SpecificationObjectHash,
          class SpecificationObjectEqual>
bool LinearizabilityCheckerRecursive<
    LinearSpecificationObject, SpecificationObjectHash,
    SpecificationObjectEqual>::Check(const std::vector<HistoryEvent>& history) {
  const auto event_info = ltest::detail::BuildRecursiveHistoryEventInfo(history);
  std::vector<const Method*> method_by_event(history.size(), nullptr);

  for (size_t i = 0; i < event_info.size(); ++i) {
    if (!event_info[i].is_invoke) continue;

    auto method_it = specification_methods.find(std::string(event_info[i].name));
    if (method_it == specification_methods.end()) {
      throw std::runtime_error("No method in spec for: " +
                               std::string(event_info[i].name));
    }
    method_by_event[i] = &method_it->second;
  }

  struct Adapter {
    const std::vector<ltest::detail::RecursiveHistoryEventInfo>& events;
    const std::vector<const Method*>& methods;

    [[nodiscard]] bool IsInvoke(size_t index) const {
      return events[index].is_invoke;
    }

    [[nodiscard]] bool IsResponse(size_t index) const {
      return events[index].is_response;
    }

    [[nodiscard]] size_t ResponseIndex(size_t index) const {
      return events[index].response_index;
    }

    bool TryApply(size_t index, LinearSpecificationObject& state) const {
      const auto& event = events[index];
      ValueWrapper expected = (*methods[index])(&state, event.args);
      if (event.response_index == ltest::detail::kNoResponseIndex) {
        return true;
      }

      return expected == *events[event.response_index].result;
    }
  };

  return ltest::detail::RunRecursiveLinearizationSearchWithCache<
      SpecificationObjectHash, SpecificationObjectEqual>(
      history.size(), first_state, Adapter{event_info, method_by_event});
}
