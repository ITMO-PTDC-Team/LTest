#pragma once

#include <cstddef>
#include <concepts>
#include <functional>
#include <unordered_set>
#include <utility>
#include <vector>

namespace ltest::detail {

inline constexpr size_t kNoResponseIndex = static_cast<size_t>(-1);

// Shared recursive linearizability search.
//
// Adapter contract:
//   bool IsInvoke(size_t event_index) const;
//   bool IsResponse(size_t event_index) const;
//   size_t ResponseIndex(size_t invoke_index) const;
//   bool TryApply(size_t invoke_index, State& state) const;
template <class State>
struct AcceptAnyCompletedLinearization {
  bool operator()(const State&) const { return true; }
};

template <class State, class Hash, class Equal>
concept SearchCacheableState = requires(const State& lhs, const State& rhs) {
  { Hash{}(lhs) } -> std::convertible_to<size_t>;
  { Equal{}(lhs, rhs) } -> std::convertible_to<bool>;
};

template <class State>
struct SearchCacheKey {
  std::vector<unsigned char> linearized;
  State state;
};

template <class State, class Hash>
struct SearchCacheKeyHash {
  size_t operator()(const SearchCacheKey<State>& key) const {
    size_t h = 1469598103934665603ull;
    for (unsigned char v : key.linearized) {
      h ^= static_cast<size_t>(v);
      h *= 1099511628211ull;
    }
    h ^= Hash{}(key.state) + 0x9e3779b97f4a7c15ull +
         (h << 6) + (h >> 2);
    return h;
  }
};

template <class State, class Equal>
struct SearchCacheKeyEqual {
  bool operator()(const SearchCacheKey<State>& lhs,
                  const SearchCacheKey<State>& rhs) const {
    return lhs.linearized == rhs.linearized &&
           Equal{}(lhs.state, rhs.state);
  }
};

template <class State, class Hash, class Equal, bool Enabled>
struct SearchFailureCache;

template <class State, class Hash, class Equal>
struct SearchFailureCache<State, Hash, Equal, false> {
  bool Contains(const std::vector<unsigned char>&, const State&) const {
    return false;
  }

  void Insert(const std::vector<unsigned char>&, const State&) {}
};

template <class State, class Hash, class Equal>
struct SearchFailureCache<State, Hash, Equal, true> {
  bool Contains(const std::vector<unsigned char>& linearized,
                const State& state) const {
    return failed.contains(SearchCacheKey<State>{linearized, state});
  }

  void Insert(const std::vector<unsigned char>& linearized,
              const State& state) {
    failed.insert(SearchCacheKey<State>{linearized, state});
  }

  std::unordered_set<SearchCacheKey<State>,
                     SearchCacheKeyHash<State, Hash>,
                     SearchCacheKeyEqual<State, Equal>>
      failed;
};

template <class Adapter>
bool CanDropPendingInvoke(const Adapter& adapter, size_t index) {
  if constexpr (requires { adapter.CanDropPendingInvoke(index); }) {
    return adapter.CanDropPendingInvoke(index);
  }
  return false;
}

template <class State, class Adapter, class OnComplete,
          class Hash = std::hash<State>,
          class Equal = std::equal_to<State>>
bool RunRecursiveLinearizationSearchImpl(size_t history_size,
                                         const State& initial_state,
                                         const Adapter& adapter,
                                         const OnComplete& on_complete) {
  if (history_size == 0) return true;

  std::vector<unsigned char> linearized(history_size, false);
  SearchFailureCache<State, Hash, Equal,
                     SearchCacheableState<State, Hash, Equal>>
      failed_cache;

  std::function<bool(const State&, size_t)> step;
  step = [&](const State& state, size_t linearized_count) -> bool {
    if (linearized_count == history_size) return on_complete(state);
    if (failed_cache.Contains(linearized, state)) return false;

    for (size_t i = 0; i < history_size; ++i) {
      if (linearized[i]) continue;

      // A not-yet-linearized response closes the minimal concurrent section.
      if (adapter.IsResponse(i)) break;
      if (!adapter.IsInvoke(i)) continue;

      const size_t response_index = adapter.ResponseIndex(i);
      const bool has_response = response_index != kNoResponseIndex;
      if (has_response && linearized[response_index]) continue;

      if (!has_response && CanDropPendingInvoke(adapter, i)) {
        linearized[i] = true;
        if (step(state, linearized_count + 1)) return true;
        linearized[i] = false;
      }

      State next_state = state;
      if (!adapter.TryApply(i, next_state)) continue;

      linearized[i] = true;
      size_t next_linearized_count = linearized_count + 1;
      if (has_response) {
        linearized[response_index] = true;
        ++next_linearized_count;
      }

      if (step(next_state, next_linearized_count)) return true;

      linearized[i] = false;
      if (has_response) linearized[response_index] = false;
    }

    failed_cache.Insert(linearized, state);
    return false;
  };

  return step(initial_state, 0);
}

template <class Hash, class Equal, class State, class Adapter, class OnComplete>
bool RunRecursiveLinearizationSearchWithCache(size_t history_size,
                                              const State& initial_state,
                                              const Adapter& adapter,
                                              const OnComplete& on_complete) {
  return RunRecursiveLinearizationSearchImpl<State, Adapter, OnComplete, Hash,
                                             Equal>(
      history_size, initial_state, adapter, on_complete);
}

template <class State, class Adapter, class OnComplete>
bool RunRecursiveLinearizationSearch(size_t history_size,
                                     const State& initial_state,
                                     const Adapter& adapter,
                                     const OnComplete& on_complete) {
  return RunRecursiveLinearizationSearchImpl(
      history_size, initial_state, adapter, on_complete);
}

template <class Hash, class Equal, class State, class Adapter>
bool RunRecursiveLinearizationSearchWithCache(size_t history_size,
                                              const State& initial_state,
                                              const Adapter& adapter) {
  return RunRecursiveLinearizationSearchWithCache<Hash, Equal>(
      history_size, initial_state, adapter,
      AcceptAnyCompletedLinearization<State>{});
}

template <class State, class Adapter>
bool RunRecursiveLinearizationSearch(size_t history_size,
                                     const State& initial_state,
                                     const Adapter& adapter) {
  return RunRecursiveLinearizationSearch(
      history_size, initial_state, adapter,
      AcceptAnyCompletedLinearization<State>{});
}

}  // namespace ltest::detail
