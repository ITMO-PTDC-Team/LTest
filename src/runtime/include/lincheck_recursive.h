#pragma once

#include <functional>
#include <numeric>

#include "lincheck.h"

// This is the simplest wgl version, it doesn't contain any optimizations, it's
// slow but useful for stress tests of other implementations
template <class LinearSpecificationObject,
          class SpecificationObjectHash = std::hash<LinearSpecificationObject>,
          class SpecificationObjectEqual =
              std::equal_to<LinearSpecificationObject>>
struct LinearizabilityCheckerRecursive : ModelChecker {
  LinearizabilityCheckerRecursive() = delete;

  LinearizabilityCheckerRecursive(
      std::map<MethodName, std::function<int(LinearSpecificationObject*)>>
          specification_methods,
      LinearSpecificationObject first_state);

  bool Check(const std::vector<std::variant<Invoke, Response>>& fixed_history)
      override;

 private:
  std::map<MethodName, std::function<int(LinearSpecificationObject*)>>
      specification_methods;
  LinearSpecificationObject first_state;
};

template <class LinearSpecificationObject, class SpecificationObjectHash,
          class SpecificationObjectEqual>
LinearizabilityCheckerRecursive<LinearSpecificationObject,
                                SpecificationObjectHash,
                                SpecificationObjectEqual>::
    LinearizabilityCheckerRecursive(
        std::map<MethodName, std::function<int(LinearSpecificationObject*)>>
            specification_methods,
        LinearSpecificationObject first_state)
    : specification_methods(specification_methods), first_state(first_state) {
  if (!std::is_copy_assignable_v<LinearSpecificationObject>) {
    // TODO: should do it in the compile time
    throw std::invalid_argument(
        "LinearSpecificationObject type have to be is_copy_assignable_v");
  }
}

template <class LinearSpecificationObject, class SpecificationObjectHash,
          class SpecificationObjectEqual>
bool LinearizabilityCheckerRecursive<LinearSpecificationObject,
                                     SpecificationObjectHash,
                                     SpecificationObjectEqual>::
    Check(const std::vector<std::variant<Invoke, Response>>& history) {
  // It's a crunch, but it's required because the semantics of this
  // implementation must be the same as the semantics of the non-recursive
  // implementation
  if (history.empty()) {
    return true;
  }
  std::map<size_t, size_t> inv_res = get_inv_res_mapping(history);

  std::function<bool(const std::vector<std::variant<Invoke, Response>>&,
                     std::vector<bool>&, LinearSpecificationObject)>
      recursive_step;

  recursive_step =
      [&](const std::vector<std::variant<Invoke, Response>>& history,
          std::vector<bool>& linearized,
          LinearSpecificationObject data_structure_state) -> bool {
    // the fixed_history is empty
    if (std::reduce(linearized.begin(), linearized.end(), true,
                    std::bit_and<>())) {
      return true;
    }

    // walk all minimal operations
    for (size_t i = 0; i < history.size(); ++i) {
      // we could think that fixed_history doesn't contain events that already
      // have been linearized
      if (linearized[i]) {
        continue;
      }

      // all next operations are not minimal
      if (history[i].index() == 1) {
        break;
      }

      Invoke minimal_op = std::get<Invoke>(history[i]);
      auto method =
          specification_methods.find(minimal_op.GetTask().GetName())->second;

      LinearSpecificationObject data_structure_state_copy =
          data_structure_state;
      // state is already have been copied, because it's the argument of the
      // lambda
      int res = method(&data_structure_state_copy);
      // If invoke doesn't have a response we can't check the response
      if (inv_res.find(i) == inv_res.end()) {
        linearized[i] = true;
        if (recursive_step(history, linearized, data_structure_state_copy)) {
          return true;
        }
        linearized[i] = false;
        continue;
      }

      if (res == minimal_op.GetTask().GetRetVal()) {
        linearized[i] = true;
        assert(inv_res.find(i) != inv_res.end());
        linearized[inv_res[i]] = true;

        if (recursive_step(history, linearized, data_structure_state_copy)) {
          return true;
        } else {
          linearized[i] = false;
          linearized[inv_res[i]] = false;
        }
      }
    }

    return false;
  };

  std::vector<bool> linearized(history.size(), false);
  return recursive_step(history, linearized, first_state);
}
