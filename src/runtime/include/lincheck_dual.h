#pragma once

#include <functional>
#include <iostream>
#include <numeric>

#include "lincheck.h"

// TODO: these code should be integrated with the lincheck checker, but
// unfortunately it's not obvious how to cache results of blocking calls(see
// docs for details), so now it's a separate struct
struct BlockingMethod {
  // Starts a new coroutine and the blocking method inside
  virtual void StartRequest() = 0;
  // Returns whether followup finished successfully
  virtual bool IsFinished() = 0;
  // Returns value that was returned by the blocking method
  virtual int GetResult() = 0;
  virtual ~BlockingMethod() = default;
};

// PromiseType have to be awaitable object
// BlockingMethodWrapper is a wrapper that wraps a coroutine promise(aka result
// of a blocking method)
template <class PromiseType>
struct BlockingMethodWrapper : BlockingMethod {
  explicit BlockingMethodWrapper(PromiseType promise)
      : promise(promise), coroutine_state(std::nullopt) {}

  // starts a new coroutine and save the coroutine state
  void StartRequest() override {
    coroutine_state = std::move(StartRequestCoroutine());
  }

  bool IsFinished() override { return result != std::nullopt; }

  int GetResult() override {
    assert(result != std::nullopt);
    return result.value();
  }

  // Can't copy the blocking method, because it's unclear what it means if we
  // can't copy the coroutines state
  BlockingMethodWrapper(BlockingMethodWrapper& other) = delete;

  ~BlockingMethodWrapper() override {
    //    std::cout << "address " << coroutine_state->h.address() << std::endl;
    if (coroutine_state->h) {
      // TODO: Now there is a memory leak. If I use destroy then it's a heap use
      // after free, it's not clear why.
      //      coroutine_state->h.destroy();
    }
  }

 private:
  struct CoroutineResponse;

  // coroutine to await on the object inside
  CoroutineResponse StartRequestCoroutine() { result = co_await promise; }

  // have to have this boilerplate, it's required by the c++ standard
  struct CoroutineResponse {
    struct promise_type {
      CoroutineResponse get_return_object() {
        return {.h = std::coroutine_handle<promise_type>::from_promise(*this)};
      }
      std::suspend_never initial_suspend() { return {}; }
      std::suspend_never final_suspend() noexcept { return {}; }
      void unhandled_exception() {}
    };

    std::coroutine_handle<promise_type> h;
  };

  PromiseType promise;
  std::optional<CoroutineResponse> coroutine_state;
  std::optional<int> result;
};

// This is the modified wg algorithm version, see docs for more details
template <class LinearSpecificationObject>
struct LinearizabilityDualChecker {
  using BlockingMethodFactory = std::function<std::shared_ptr<BlockingMethod>(
      LinearSpecificationObject*, void* args)>;

  using NonBlockingMethod =
      std::function<int(LinearSpecificationObject*, void* args)>;

  using Method = std::variant<NonBlockingMethod, BlockingMethodFactory>;
  using MethodMap = std::map<MethodName, Method>;

  LinearizabilityDualChecker() = delete;

  LinearizabilityDualChecker(MethodMap specification_methods,
                             LinearSpecificationObject first_state);

  bool Check(const std::vector<HistoryEvent>& fixed_history);

 private:
  // ReproduceSeqHistory applies all events from the sequential history to the
  // first_state and returns obtained struct state and the stack of blocking
  // methods that were invoked
  std::pair<LinearSpecificationObject,
            std::map<size_t, std::shared_ptr<BlockingMethod>>>
  ReproduceSeqHistory(
      std::vector<std::pair<HistoryEvent, size_t>>& seq_history);

  std::map<MethodName, Method> specification_methods;
  LinearSpecificationObject first_state;
};

template <class LinearSpecificationObject>
LinearizabilityDualChecker<LinearSpecificationObject>::
    LinearizabilityDualChecker(
        LinearizabilityDualChecker::MethodMap specification_methods,
        LinearSpecificationObject first_state)
    : specification_methods(specification_methods), first_state(first_state) {
  if (!std::is_copy_assignable_v<LinearSpecificationObject>) {
    // TODO: should do it in the compile time
    throw std::invalid_argument(
        "LinearSpecificationObject type have to be is_copy_assignable_v");
  }
}

template <class LinearSpecificationObject>
bool LinearizabilityDualChecker<LinearSpecificationObject>::Check(
    const std::vector<HistoryEvent>& history) {
  // It's a crunch, but it's required because the semantics of this
  // implementation must be the same as the semantics of the non-recursive
  // implementation
  if (history.empty()) {
    return true;
  }

  // response -> invoke
  std::map<size_t, size_t> inv_res = get_inv_res_full_mapping(history);
  // follow_up_response -> request_invoke
  std::map<size_t, size_t> followup_request =
      get_followup_res_request_inv_mapping(history);
  // it can be a stack, but it's easier to use map: index -> method
  std::map<size_t, std::shared_ptr<BlockingMethod>> dual_requests;
  // current history
  std::vector<std::pair<HistoryEvent, size_t>> seq_history;

  std::function<bool(const std::vector<HistoryEvent>&, std::vector<bool>&,
                     LinearSpecificationObject)>
      recursive_step;

  std::function<LinearSpecificationObject(std::vector<size_t>&&,
                                          std::vector<bool>&)>
      fix_history_update_duals =
          [&](std::vector<size_t>&& indexes, std::vector<bool>& linearized) {
            for (auto index : indexes) {
              seq_history.pop_back();
              linearized[index] = false;
            }
            // have to reproduce the state from the start, because it's
            // impossible to copy a blocking method, so dual_requests might be
            // corrupted by the recursive_step call
            auto [state, duals] = ReproduceSeqHistory(seq_history);
            dual_requests = duals;

            return state;
          };

  recursive_step = [&](const std::vector<HistoryEvent>& history,
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
      if (std::holds_alternative<Response>(history[i]) ||
          std::holds_alternative<RequestResponse>(history[i]) ||
          std::holds_alternative<FollowUpResponse>(history[i])) {
        break;
      }

      if (std::holds_alternative<Invoke>(history[i])) {
        // Nonblocking method case, same with the case in default recursive
        // checker
        Invoke minimal_op = std::get<Invoke>(history[i]);
        NonBlockingMethod method = std::get<NonBlockingMethod>(
            specification_methods.find(minimal_op.GetTask().GetName())->second);

        LinearSpecificationObject data_structure_state_copy =
            data_structure_state;
        // state is already have been copied, because it's the argument of the
        // lambda
        int res =
            method(&data_structure_state_copy, minimal_op.GetTask().GetArgs());

        // If invoke doesn't have a response we can't check the response
        if (inv_res.find(i) == inv_res.end()) {
          linearized[i] = true;
          seq_history.emplace_back(history[i], i);
          if (recursive_step(history, linearized, data_structure_state_copy)) {
            return true;
          }
          data_structure_state = fix_history_update_duals({i}, linearized);
          continue;
        }

        // Have a response, try to linearize
        if (res == minimal_op.GetTask().GetRetVal()) {
          linearized[i] = true;
          seq_history.emplace_back(history[i], i);
          assert(inv_res.find(i) != inv_res.end());
          linearized[inv_res[i]] = true;
          seq_history.emplace_back(history[inv_res[i]], inv_res[i]);

          if (recursive_step(history, linearized, data_structure_state_copy)) {
            return true;
          }
          // go back and reproduce state and duals
          data_structure_state =
              fix_history_update_duals({i, inv_res[i]}, linearized);
        }
      } else if (std::holds_alternative<RequestInvoke>(history[i])) {
        // Blocking method, try to linearize it, because we don't need to check
        // an answer in the request part answer have to be ready only when we
        // will be considering the corresponding follow up part
        RequestInvoke minimal_op = std::get<RequestInvoke>(history[i]);
        BlockingMethodFactory mf = std::get<BlockingMethodFactory>(
            specification_methods.find(minimal_op.GetTask().GetName())->second);

        LinearSpecificationObject data_structure_state_copy =
            data_structure_state;

        // Get out blocking method and save it to the map(stack)
        std::shared_ptr<BlockingMethod> method =
            mf(&data_structure_state_copy, minimal_op.GetTask().GetArgs());
        method->StartRequest();
        dual_requests[i] = method;

        // TODO: better to write an if with res found/not found, because it's
        // easier to read
        linearized[i] = true;
        seq_history.emplace_back(history[i], i);
        if (inv_res.find(i) != inv_res.end()) {
          linearized[inv_res[i]] = true;
          seq_history.emplace_back(history[inv_res[i]], inv_res[i]);
        }

        if (recursive_step(history, linearized, data_structure_state_copy)) {
          return true;
        }
        // go back and reproduce state and duals
        seq_history.pop_back();
        linearized[i] = false;
        if (inv_res.find(i) != inv_res.end()) {
          linearized[inv_res[i]] = false;
          seq_history.pop_back();
        }
        std::tie(data_structure_state, dual_requests) =
            ReproduceSeqHistory(seq_history);
      } else if (std::holds_alternative<FollowUpInvoke>(history[i])) {
        // Blocking method follow-up section, have to check the return value
        // there
        FollowUpInvoke minimal_op = std::get<FollowUpInvoke>(history[i]);
        size_t followup_response_index = inv_res[i];
        size_t request_index = followup_request[followup_response_index];
        assert(dual_requests.find(request_index) != dual_requests.end());
        std::shared_ptr<BlockingMethod> method = dual_requests[request_index];

        // If the method doesn't ready just keep execution
        if (method->IsFinished() &&
            method->GetResult() == minimal_op.GetTask().GetRetVal()) {
          linearized[i] = true;
          // TODO: might not have an answer, should be able to check unfinished
          // histories
          seq_history.emplace_back(history[i], i);
          linearized[inv_res[i]] = true;
          seq_history.emplace_back(history[inv_res[i]], inv_res[i]);

          if (recursive_step(history, linearized, data_structure_state)) {
            return true;
          }
          data_structure_state =
              fix_history_update_duals({i, inv_res[i]}, linearized);
        }
      }
    }

    return false;
  };

  std::vector<bool> linearized(history.size(), false);
  return recursive_step(history, linearized, first_state);
}

template <class LinearSpecificationObject>
std::pair<LinearSpecificationObject,
          std::map<size_t, std::shared_ptr<BlockingMethod>>>
LinearizabilityDualChecker<LinearSpecificationObject>::ReproduceSeqHistory(
    std::vector<std::pair<HistoryEvent, size_t>>& seq_history) {
  LinearSpecificationObject state = first_state;
  std::map<size_t, std::shared_ptr<BlockingMethod>> new_stack;

  for (auto event_pair : seq_history) {
    if (std::holds_alternative<Invoke>(event_pair.first)) {
      Invoke op = std::get<Invoke>(event_pair.first);

      NonBlockingMethod method = std::get<NonBlockingMethod>(
          specification_methods.find(op.GetTask().GetName())->second);

      method(&state, op.GetTask().GetArgs());
    } else if (std::holds_alternative<RequestInvoke>(event_pair.first)) {
      RequestInvoke op = std::get<RequestInvoke>(event_pair.first);

      BlockingMethodFactory mf = std::get<BlockingMethodFactory>(
          specification_methods.find(op.GetTask().GetName())->second);
      std::shared_ptr<BlockingMethod> method =
          mf(&state, op.GetTask().GetArgs());
      method->StartRequest();
      new_stack[event_pair.second] = method;
    }
  }

  return {state, new_stack};
}
