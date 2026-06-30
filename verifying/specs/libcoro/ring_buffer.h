#pragma once

#include <cstdint>
#include <deque>
#include <optional>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <coro/expected.hpp>
#include <coro/ring_buffer.hpp>

#include "../../../runtime/include/lincheck_dual.h"
#include "../../../runtime/include/value_wrapper.h"
#include "../../../runtime/include/workload_policy.h"

namespace detail {

template <class E>
static inline std::string EnumToStr(E e) {
  using U = std::underlying_type_t<E>;
  return std::to_string(static_cast<U>(e));
}

}  // namespace detail

// ---- ValueWrapper support ----
// Print/compare enums robustly (don’t depend on enumerator names)
template <>
inline ToStringFunc GetDefaultToString<coro::ring_buffer_result::produce>() {
  return [](const ValueWrapper& a) -> std::string {
    return "ring_buffer_produce(" +
           detail::EnumToStr(a.GetValue<coro::ring_buffer_result::produce>()) + ")";
  };
}
template <>
inline CompFunc GetDefaultCompator<coro::ring_buffer_result::produce>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<coro::ring_buffer_result::produce>() ==
           b.GetValue<coro::ring_buffer_result::produce>();
  };
}

template <>
inline ToStringFunc GetDefaultToString<coro::ring_buffer_result::consume>() {
  return [](const ValueWrapper& a) -> std::string {
    return "ring_buffer_consume(" +
           detail::EnumToStr(a.GetValue<coro::ring_buffer_result::consume>()) + ")";
  };
}
template <>
inline CompFunc GetDefaultCompator<coro::ring_buffer_result::consume>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<coro::ring_buffer_result::consume>() ==
           b.GetValue<coro::ring_buffer_result::consume>();
  };
}

template <>
inline ToStringFunc
GetDefaultToString<tl::expected<int, coro::ring_buffer_result::consume>>() {
  return [](const ValueWrapper& a) -> std::string {
    auto e = a.GetValue<tl::expected<int, coro::ring_buffer_result::consume>>();
    if (e.has_value()) {
      return "ok(" + std::to_string(e.value()) + ")";
    }
    return "err(" + detail::EnumToStr(e.error()) + ")";
  };
}

template <>
inline CompFunc
GetDefaultCompator<tl::expected<int, coro::ring_buffer_result::consume>>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    auto ea = a.GetValue<tl::expected<int, coro::ring_buffer_result::consume>>();
    auto eb = b.GetValue<tl::expected<int, coro::ring_buffer_result::consume>>();

    if (ea.has_value() != eb.has_value()) return false;
    if (ea.has_value()) return ea.value() == eb.value();
    return ea.error() == eb.error();
  };
}

namespace spec {

struct LibcoroRingBufferSnapshot {
  std::size_t size{};
  bool empty{};
  bool full{};

  bool operator==(const LibcoroRingBufferSnapshot&) const = default;
};

struct LibcoroRingBufferInternalSnapshot {
  std::size_t front{};
  std::size_t back{};
  std::size_t used{};
  bool slot_full{};
  int slot_value{};

  bool operator==(const LibcoroRingBufferInternalSnapshot&) const = default;
};

}  // namespace spec

template <>
inline ToStringFunc GetDefaultToString<spec::LibcoroRingBufferSnapshot>() {
  return [](const ValueWrapper& a) -> std::string {
    const auto snap = a.GetValue<spec::LibcoroRingBufferSnapshot>();
    return "snap(" + std::to_string(snap.size) + "," +
           std::to_string(snap.empty) + "," +
           std::to_string(snap.full) + ")";
  };
}

template <>
inline CompFunc GetDefaultCompator<spec::LibcoroRingBufferSnapshot>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<spec::LibcoroRingBufferSnapshot>() ==
           b.GetValue<spec::LibcoroRingBufferSnapshot>();
  };
}

template <>
inline ToStringFunc GetDefaultToString<spec::LibcoroRingBufferInternalSnapshot>() {
  return [](const ValueWrapper& a) -> std::string {
    const auto snap = a.GetValue<spec::LibcoroRingBufferInternalSnapshot>();
    return "isnap(" + std::to_string(snap.front) + "," +
           std::to_string(snap.back) + "," +
           std::to_string(snap.used) + "," +
           std::to_string(snap.slot_full) + "," +
           std::to_string(snap.slot_value) + ")";
  };
}

template <>
inline CompFunc GetDefaultCompator<spec::LibcoroRingBufferInternalSnapshot>() {
  return [](const ValueWrapper& a, const ValueWrapper& b) -> bool {
    return a.GetValue<spec::LibcoroRingBufferInternalSnapshot>() ==
           b.GetValue<spec::LibcoroRingBufferInternalSnapshot>();
  };
}

namespace spec {

// Sequential spec for coro::ring_buffer<int, 1> (bounded).
//
// Important: waiters are modeled as LIFO stacks to match libcoro implementation:
// - produce_waiters is Treiber-like (exchange/push-to-head), pop is LIFO
// - consume_waiters same
//
// We only model normal produce/consume (no notify_* / shutdown API in this target):
// - produce() completes with ring_buffer_result::produce::produced when it succeeds.
// - consume() completes with expected<int, consume> containing value.
//
// Dual split for both methods: request + followup.
struct LibcoroRingBuffer {
  static constexpr std::size_t kCap = 1;

  // Buffer contents (FIFO by element order).
  std::deque<int> elems;
  std::size_t front_cursor{0};
  std::size_t back_cursor{0};

  // Waiting producers: stack of (op_id, value)  (LIFO)
  std::vector<std::pair<int, int>> waiting_producers;

  // Waiting consumers: stack of op_id (LIFO)
  std::vector<int> waiting_consumers;

  // Ready maps for followup
  std::unordered_set<int> ready_produce;      // op_id
  std::unordered_map<int, int> ready_consume; // op_id -> value

  bool operator==(const LibcoroRingBuffer&) const = default;

  static ltest::WorkloadPolicy GetWorkloadPolicy() {
    ltest::WorkloadPolicy p;

    // If consumers are waiting, reserve thread(s) for producers.
    {
      ltest::ReserveRule r;
      r.wait_method = "consume";
      r.progress_methods = {"produce"};
      r.reserve_threads = 1;
      p.reserve.push_back(std::move(r));
    }

    // If producers are waiting (buffer full), reserve thread(s) for consumers.
    {
      ltest::ReserveRule r;
      r.wait_method = "produce";
      r.progress_methods = {"consume"};
      r.reserve_threads = 1;
      p.reserve.push_back(std::move(r));
    }

    return p;
  }

  // Helper: if buffer has element and there is a waiting consumer, match one (LIFO)
  void TryWakeConsumerFromBuffer() {
    if (elems.empty()) return;
    if (waiting_consumers.empty()) return;

    int cid = waiting_consumers.back();
    waiting_consumers.pop_back();

    int v = elems.front();
    elems.pop_front();
    ++back_cursor;
    ready_consume[cid] = v;
  }

  // Helper: if there is space and a waiting producer, admit one producer (LIFO)
  void TryAdmitProducerToBuffer() {
    if (elems.size() >= kCap) return;
    if (waiting_producers.empty()) return;

    auto [pid, v] = waiting_producers.back();
    waiting_producers.pop_back();

    elems.push_back(v);
    ++front_cursor;
    ready_produce.insert(pid);

    // libcoro produce() calls try_resume_consumers() after producing,
    // so it may immediately wake a consumer if any are waiting.
    TryWakeConsumerFromBuffer();
  }

  // ----- request phases -----
  void RequestProduce(void* args, int op_id) {
    auto* tup = reinterpret_cast<std::tuple<int>*>(args);
    int v = std::get<0>(*tup);

    if (elems.size() < kCap) {
      elems.push_back(v);
      ++front_cursor;
      ready_produce.insert(op_id);

      // After producing, implementation tries to resume consumers.
      TryWakeConsumerFromBuffer();
    } else {
      waiting_producers.push_back({op_id, v});
    }
  }

  void RequestConsume(int op_id) {
    if (!elems.empty()) {
      int v = elems.front();
      elems.pop_front();
      ++back_cursor;
      ready_consume[op_id] = v;

      // After consuming, implementation tries to resume producers.
      TryAdmitProducerToBuffer();
    } else {
      waiting_consumers.push_back(op_id);
    }
  }

  // ----- follow-up phases -----
  std::optional<ValueWrapper> FollowUpProduce(int op_id) {
    if (!ready_produce.contains(op_id)) return std::nullopt;
    ready_produce.erase(op_id);
    return ValueWrapper(coro::ring_buffer_result::produce::produced);
  }

  std::optional<ValueWrapper> FollowUpConsume(int op_id) {
    auto it = ready_consume.find(op_id);
    if (it == ready_consume.end()) return std::nullopt;

    int v = it->second;
    ready_consume.erase(it);

    tl::expected<int, coro::ring_buffer_result::consume> e{v};
    return ValueWrapper(e);
  }

  ValueWrapper Size(void* /*args*/) {
    return ValueWrapper(static_cast<std::size_t>(elems.size()));
  }

  ValueWrapper Empty(void* /*args*/) {
    return ValueWrapper(elems.empty());
  }

  ValueWrapper Full(void* /*args*/) {
    return ValueWrapper(elems.size() == kCap);
  }

  ValueWrapper Snapshot(void* /*args*/) {
    return ValueWrapper(LibcoroRingBufferSnapshot{
        .size = elems.size(),
        .empty = elems.empty(),
        .full = elems.size() == kCap,
    });
  }

  ValueWrapper InternalSnapshot(void* /*args*/) {
    const bool slot_full = !elems.empty();
    return ValueWrapper(LibcoroRingBufferInternalSnapshot{
        .front = front_cursor,
        .back = back_cursor,
        .used = elems.size(),
        .slot_full = slot_full,
        .slot_value = slot_full ? elems.front() : 0,
    });
  }

  static auto GetDualMethods() {
    using S = LibcoroRingBuffer;
    DualMethodMap<S> m;

    DualRequestMethod<S> prod_req = [](S* s, void* args, int op_id) {
      s->RequestProduce(args, op_id);
    };
    DualFollowUpMethod<S> prod_fol = [](S* s, void*, int op_id) {
      return s->FollowUpProduce(op_id);
    };

    DualRequestMethod<S> cons_req = [](S* s, void*, int op_id) {
      s->RequestConsume(op_id);
    };
    DualFollowUpMethod<S> cons_fol = [](S* s, void*, int op_id) {
      return s->FollowUpConsume(op_id);
    };

    m.emplace("produce", DualBlockingMethod<S>{prod_req, prod_fol});
    m.emplace("consume", DualBlockingMethod<S>{cons_req, cons_fol});
    m.emplace("size", DualNonBlockingMethod<S>{
                          [](S* s, void* args) {
                            return s->Size(args);
                          }});
    m.emplace("empty", DualNonBlockingMethod<S>{
                           [](S* s, void* args) {
                             return s->Empty(args);
                           }});
    m.emplace("full", DualNonBlockingMethod<S>{
                          [](S* s, void* args) {
                            return s->Full(args);
                          }});
    m.emplace("snapshot", DualNonBlockingMethod<S>{
                              [](S* s, void* args) {
                                return s->Snapshot(args);
                              }});
    m.emplace("internal_snapshot", DualNonBlockingMethod<S>{
                                       [](S* s, void* args) {
                                         return s->InternalSnapshot(args);
                                       }});
    return m;
  }
};

} // namespace spec

namespace std {

template <>
struct hash<spec::LibcoroRingBuffer> {
  size_t operator()(const spec::LibcoroRingBuffer& s) const {
    size_t h = 0;
    auto mix = [&](size_t v) {
      h ^= v + 0x9e3779b97f4a7c15ull + (h << 6) + (h >> 2);
    };

    mix(std::hash<std::size_t>{}(s.front_cursor));
    mix(std::hash<std::size_t>{}(s.back_cursor));

    for (int v : s.elems) {
      mix(std::hash<int>{}(v));
    }
    mix(0x20001u);

    for (const auto& [op_id, value] : s.waiting_producers) {
      mix(std::hash<int>{}(op_id));
      mix(std::hash<int>{}(value));
    }
    mix(0x20002u);

    for (int op_id : s.waiting_consumers) {
      mix(std::hash<int>{}(op_id));
    }
    mix(0x20003u);

    size_t ready_produce_hash = 0;
    for (int op_id : s.ready_produce) {
      ready_produce_hash ^= std::hash<int>{}(op_id) + 0x85ebca6bu;
    }
    mix(ready_produce_hash);

    size_t ready_consume_hash = 0;
    for (const auto& [op_id, value] : s.ready_consume) {
      ready_consume_hash ^= std::hash<int>{}(op_id) ^
                            (std::hash<int>{}(value) + 0xc2b2ae35u);
    }
    mix(ready_consume_hash);

    return h;
  }
};

}  // namespace std
