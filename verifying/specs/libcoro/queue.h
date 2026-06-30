#pragma once

#include <cstdint>
#include <deque>
#include <optional>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <string>

#include <coro/queue.hpp>
#include <coro/expected.hpp>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace detail {

// Print enum as integer (robust across libcoro versions)
template <class E>
std::string EnumToStr(E e) {
  using U = std::underlying_type_t<E>;
  return std::to_string(static_cast<U>(e));
}

} // namespace detail

// ---- ValueWrapper support for libcoro result types ----
template <>
inline ToStringFunc GetDefaultToString<coro::queue_produce_result>() {
  return [](const ValueWrapper& a) -> std::string {
    return "queue_produce_result(" +
           detail::EnumToStr(a.GetValue<coro::queue_produce_result>()) + ")";
  };
}

template <>
inline CompFunc GetDefaultCompator<coro::queue_produce_result>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<coro::queue_produce_result>() ==
           b.GetValue<coro::queue_produce_result>();
  };
}

template <>
inline ToStringFunc GetDefaultToString<coro::queue_consume_result>() {
  return [](const ValueWrapper& a) -> std::string {
    return "queue_consume_result(" +
           detail::EnumToStr(a.GetValue<coro::queue_consume_result>()) + ")";
  };
}

template <>
inline CompFunc GetDefaultCompator<coro::queue_consume_result>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<coro::queue_consume_result>() ==
           b.GetValue<coro::queue_consume_result>();
  };
}

template <>
inline ToStringFunc
GetDefaultToString<tl::expected<int, coro::queue_consume_result>>() {
  return [](const ValueWrapper& a) -> std::string {
    auto e = a.GetValue<tl::expected<int, coro::queue_consume_result>>();
    if (e.has_value()) {
      return "ok(" + std::to_string(e.value()) + ")";
    }
    return "err(queue_consume_result(" + detail::EnumToStr(e.error()) + "))";
  };
}

template <>
inline CompFunc
GetDefaultCompator<tl::expected<int, coro::queue_consume_result>>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    auto ea = a.GetValue<tl::expected<int, coro::queue_consume_result>>();
    auto eb = b.GetValue<tl::expected<int, coro::queue_consume_result>>();

    if (ea.has_value() != eb.has_value()) return false;
    if (ea.has_value()) return ea.value() == eb.value();
    return ea.error() == eb.error();
  };
}

namespace spec {

// Sequential spec for libcoro coro::queue<int>:
// - pop() blocks if queue empty.
// Matching for waiting pops is LIFO (stack): newest waiting pop gets next produced element.
// This matches libcoro implementation: m_waiters is a singly-linked stack.
struct LibcoroQueue {
  std::deque<int> elems;                  // buffered elements (FIFO queue)
  std::deque<int> waiting_pops;           // op_id list (LIFO stack: use back)
  std::unordered_map<int, int> ready_pop; // op_id -> value
  std::unordered_set<int> push_done;      // push followup ready after request

  bool operator==(const LibcoroQueue&) const = default;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;
    ltest::ReserveRule r;
    r.wait_method = "pop";
    r.progress_methods = {"push"};
    r.reserve_threads = 1;
    p.reserve.push_back(std::move(r));
    return p;
  }

  // ----- request phase -----
  void RequestPush(int op_id, int v) {
    push_done.insert(op_id);

    if (!waiting_pops.empty()) {
      // LIFO: newest waiter
      int pop_id = waiting_pops.back();
      waiting_pops.pop_back();
      ready_pop[pop_id] = v;
    } else {
      elems.push_back(v);
    }
  }

  void RequestPop(int op_id) {
    if (!elems.empty()) {
      int v = elems.front();
      elems.pop_front();
      ready_pop[op_id] = v;
    } else {
      // push waiter to the "top of stack"
      waiting_pops.push_back(op_id);
    }
  }

  // ----- follow-up phase -----
  std::optional<ValueWrapper> FollowUpPush(int op_id) {
    if (!push_done.contains(op_id)) return std::nullopt;
    push_done.erase(op_id);
    return ValueWrapper(coro::queue_produce_result::produced);
  }

  std::optional<ValueWrapper> FollowUpPop(int op_id) {
    auto it = ready_pop.find(op_id);
    if (it == ready_pop.end()) return std::nullopt;
    int v = it->second;
    ready_pop.erase(it);

    tl::expected<int, coro::queue_consume_result> e{v};
    return ValueWrapper(e);
  }

  static auto GetDualMethods() {
    using S = LibcoroQueue;
    DualMethodMap<S> m;

    DualRequestMethod<S> push_req = [](S* s, void* args, int op_id) {
      auto* tup = reinterpret_cast<std::tuple<int>*>(args);
      int v = std::get<0>(*tup);
      s->RequestPush(op_id, v);
    };
    DualFollowUpMethod<S> push_fol = [](S* s, void*, int op_id) {
      return s->FollowUpPush(op_id);
    };

    DualRequestMethod<S> pop_req = [](S* s, void*, int op_id) {
      s->RequestPop(op_id);
    };
    DualFollowUpMethod<S> pop_fol = [](S* s, void*, int op_id) {
      return s->FollowUpPop(op_id);
    };

    m.emplace("push", DualBlockingMethod<S>{push_req, push_fol});
    m.emplace("pop",  DualBlockingMethod<S>{pop_req, pop_fol});
    return m;
  }
};

} // namespace spec

namespace std {

template <>
struct hash<spec::LibcoroQueue> {
  size_t operator()(const spec::LibcoroQueue& s) const {
    size_t h = 0;
    auto mix = [&](size_t v) {
      h ^= v + 0x9e3779b97f4a7c15ull + (h << 6) + (h >> 2);
    };

    for (int v : s.elems) {
      mix(std::hash<int>{}(v));
    }
    mix(0x10001u);

    for (int id : s.waiting_pops) {
      mix(std::hash<int>{}(id));
    }
    mix(0x10002u);

    size_t ready_hash = 0;
    for (const auto& [id, value] : s.ready_pop) {
      ready_hash ^= std::hash<int>{}(id) ^
                    (std::hash<int>{}(value) + 0x9e3779b9u);
    }
    mix(ready_hash);

    size_t done_hash = 0;
    for (int id : s.push_done) {
      done_hash ^= std::hash<int>{}(id) + 0x85ebca6bu;
    }
    mix(done_hash);

    return h;
  }
};

}  // namespace std
