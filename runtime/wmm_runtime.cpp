#include "include/wmm_runtime.h"

#include <atomic>
#include <cassert>
#include <type_traits>
#include <vector>

#include "include/lib.h"
#include "include/scheduler.h"
#include "include/wmm/wmm.h"

using ltest::wmm::WmmUtils;

namespace {

template <typename T>
struct CandidateStorage {
  bool use_graph = false;
  std::vector<typename ltest::wmm::Graph::template ReadCandidate<T>> graph;
  std::vector<T> direct;
};

template <typename T>
CandidateStorage<T>& GetCandidateStorageForThread(int thread_id) {
  static std::vector<CandidateStorage<T>> storage;
  if (thread_id < 0) {
    thread_id = 0;
  }
  if (storage.size() <= static_cast<size_t>(thread_id)) {
    storage.resize(thread_id + 1);
  }
  return storage[thread_id];
}

template <typename T>
int LoadCandidatesImpl(void* addr, int order);

template <typename T>
T ApplyLoadImpl(int choice_index);

template <typename T>
int GetLocationId(void* addr, T initial_value) {
  auto& graph = ltest::wmm::ExecutionGraph::getInstance();
  (void)initial_value;
  auto* raw = reinterpret_cast<T*>(addr);
  T init_value = __atomic_load_n(raw, __ATOMIC_RELAXED);
  return graph.GetOrRegisterLocation<T>(addr, init_value, this_thread_id);
}

template <typename T>
T LoadImpl(void* addr, int order) {
  int candidates_count = LoadCandidatesImpl<T>(addr, order);
  int choice_index = __ltest_wmm_choice(candidates_count);
  return ApplyLoadImpl<T>(choice_index);
}

template <typename T>
int LoadCandidatesImpl(void* addr, int order) {
  auto& storage = GetCandidateStorageForThread<T>(this_thread_id);
  storage.graph.clear();
  storage.direct.clear();

  if (!ltest::wmm::wmm_enabled || !this_coro) {
    // read the real atomic directly
    auto* atom = reinterpret_cast<std::atomic<T>*>(addr);
    storage.direct.push_back(
        atom->load(static_cast<std::memory_order>(order)));
    log() << "[WMM][candidates][direct] thread=" << this_thread_id
        << " addr=" << addr << " order=" << order
        << " count=" << storage.direct.size()
        << " value0=" << storage.direct.front() << "\n";
    storage.use_graph = false;
    return static_cast<int>(storage.direct.size());
  }

  auto& graph = ltest::wmm::ExecutionGraph::getInstance();
  int location_id = GetLocationId<T>(addr, T{});
  storage.graph = graph.LoadCandidates<T>(
      location_id,
      this_thread_id,
      WmmUtils::OrderFromStd(static_cast<std::memory_order>(order)));
  storage.use_graph = true;
  assert(!storage.graph.empty() && "WMM graph returned no read candidates");

  log() << "[WMM][candidates][graph] thread=" << this_thread_id
        << " loc=" << location_id << " addr=" << addr << " order="
        << order << " count=" << storage.graph.size() << "\n";
  for (size_t i = 0; i < storage.graph.size(); ++i) {
    const auto& c = storage.graph[i];
    log() << "  [" << i << "] value=" << c.value
          << " write=" << c.write_event->AsString()
          << " read=" << c.read_event->AsString() << "\n";
  }

  return static_cast<int>(storage.graph.size());
}

template <typename T>
T ApplyLoadImpl(int choice_index) {
  auto& storage = GetCandidateStorageForThread<T>(this_thread_id);
  if (storage.use_graph) {
    if (choice_index < 0 ||
        static_cast<size_t>(choice_index) >= storage.graph.size()) {
      choice_index = 0;
    }
    auto& graph = ltest::wmm::ExecutionGraph::getInstance();
    T value = graph.ApplyReadCandidate<T>(storage.graph[choice_index]);
    storage.graph.clear();
    storage.use_graph = false;
    return value;
  }

  if (storage.direct.empty()) {
    return T{};
  }
  if (choice_index < 0 ||
      static_cast<size_t>(choice_index) >= storage.direct.size()) {
    choice_index = 0;
  }
  T value = storage.direct[choice_index];
  storage.direct.clear();
  storage.use_graph = false;
  return value;
}

template <typename T>
void StoreImpl(void* addr, int order, T value) {
  if (ltest::wmm::wmm_enabled && this_coro) {
    auto& graph = ltest::wmm::ExecutionGraph::getInstance();
    int location_id = GetLocationId<T>(addr, value);
    graph.Store<T>(
        location_id,
        this_thread_id,
        WmmUtils::OrderFromStd(static_cast<std::memory_order>(order)),
        value);
    return;
  }
  auto* atom = reinterpret_cast<std::atomic<T>*>(addr);
  atom->store(value, static_cast<std::memory_order>(order));
}

template <typename T>
struct CmpXchgResult {
  T old;
  bool success;
};

template <typename T>
CmpXchgResult<T> CmpXchgImpl(
    void* addr,
    T expected,
    T desired,
    int success_order,
    int failure_order,
    bool is_weak) {
  if (ltest::wmm::wmm_enabled && this_coro) {
    auto& graph = ltest::wmm::ExecutionGraph::getInstance();
    int location_id = GetLocationId<T>(addr, expected);
    T expected_copy = expected;
    auto [rmw_success, read_value] = graph.ReadModifyWrite<T>(
        location_id,
        this_thread_id,
        &expected_copy,
        desired,
        WmmUtils::OrderFromStd(static_cast<std::memory_order>(success_order)),
        WmmUtils::OrderFromStd(static_cast<std::memory_order>(failure_order)));
    return CmpXchgResult<T>{read_value, rmw_success};
  }

  auto* atom = reinterpret_cast<std::atomic<T>*>(addr);
  T expected_copy = expected;
  bool success = is_weak
                     ? atom->compare_exchange_weak(
                           expected_copy, desired,
                           static_cast<std::memory_order>(success_order),
                           static_cast<std::memory_order>(failure_order))
                     : atom->compare_exchange_strong(
                           expected_copy, desired,
                           static_cast<std::memory_order>(success_order),
                           static_cast<std::memory_order>(failure_order));
  return CmpXchgResult<T>{expected_copy, success};
}

template <typename T>
T AtomicRmwImpl(void* addr, int order, int op, T operand) {
  // wmm graph currently models only compare_exchange rmw
  // for other rmw ops fallback to real atomics 
  auto* atom = reinterpret_cast<std::atomic<T>*>(addr);
  auto order_mo = static_cast<std::memory_order>(order);
  switch (op) {
    case LTEST_RMW_ADD: return atom->fetch_add(operand, order_mo);
    case LTEST_RMW_SUB: return atom->fetch_sub(operand, order_mo);
    case LTEST_RMW_AND: return atom->fetch_and(operand, order_mo);
    case LTEST_RMW_OR: return atom->fetch_or(operand, order_mo);
    case LTEST_RMW_XOR: return atom->fetch_xor(operand, order_mo);
    case LTEST_RMW_XCHG: return atom->exchange(operand, order_mo);
    default: {
      // max/min/umax/umin через cas
      T cur = atom->load(order_mo);
      while (true) {
        T next = cur;
        switch (op) {
          case LTEST_RMW_MAX: next = (cur > operand) ? cur : operand; break;
          case LTEST_RMW_MIN: next = (cur < operand) ? cur : operand; break;
          case LTEST_RMW_UMAX: {
            using U = std::make_unsigned_t<T>;
            next = (static_cast<U>(cur) > static_cast<U>(operand)) ? cur : operand;
            break;
          }
          case LTEST_RMW_UMIN: {
            using U = std::make_unsigned_t<T>;
            next = (static_cast<U>(cur) < static_cast<U>(operand)) ? cur : operand;
            break;
          }
          default: next = operand; break;
        }
        if (atom->compare_exchange_weak(cur, next, order_mo, order_mo)) {
          return cur;
        }
      }
    }
  }
}

}  // namespace

extern "C" bool __ltest_wmm_load_i1(void* addr, int order) {
  return LoadImpl<bool>(addr, order);
}

extern "C" int8_t __ltest_wmm_load_i8(void* addr, int order) {
  return LoadImpl<int8_t>(addr, order);
}

extern "C" int16_t __ltest_wmm_load_i16(void* addr, int order) {
  return LoadImpl<int16_t>(addr, order);
}

extern "C" int32_t __ltest_wmm_load_i32(void* addr, int order) {
  return LoadImpl<int32_t>(addr, order);
}

extern "C" int64_t __ltest_wmm_load_i64(void* addr, int order) {
  return LoadImpl<int64_t>(addr, order);
}

extern "C" int __ltest_wmm_load_candidates_i1(void* addr, int order) {
  return LoadCandidatesImpl<bool>(addr, order);
}

extern "C" int __ltest_wmm_load_candidates_i8(void* addr, int order) {
  return LoadCandidatesImpl<int8_t>(addr, order);
}

extern "C" int __ltest_wmm_load_candidates_i16(void* addr, int order) {
  return LoadCandidatesImpl<int16_t>(addr, order);
}

extern "C" int __ltest_wmm_load_candidates_i32(void* addr, int order) {
  return LoadCandidatesImpl<int32_t>(addr, order);
}

extern "C" int __ltest_wmm_load_candidates_i64(void* addr, int order) {
  return LoadCandidatesImpl<int64_t>(addr, order);
}

extern "C" bool __ltest_wmm_load_apply_i1(int choice_index) {
  return ApplyLoadImpl<bool>(choice_index);
}

extern "C" int8_t __ltest_wmm_load_apply_i8(int choice_index) {
  return ApplyLoadImpl<int8_t>(choice_index);
}

extern "C" int16_t __ltest_wmm_load_apply_i16(int choice_index) {
  return ApplyLoadImpl<int16_t>(choice_index);
}

extern "C" int32_t __ltest_wmm_load_apply_i32(int choice_index) {
  return ApplyLoadImpl<int32_t>(choice_index);
}

extern "C" int64_t __ltest_wmm_load_apply_i64(int choice_index) {
  return ApplyLoadImpl<int64_t>(choice_index);
}


extern "C" void __ltest_wmm_store_i1(void* addr, int order, bool value) {
  StoreImpl<bool>(addr, order, value);
}

extern "C" void __ltest_wmm_store_i8(void* addr, int order, int8_t value) {
  StoreImpl<int8_t>(addr, order, value);
}

extern "C" void __ltest_wmm_store_i16(void* addr, int order, int16_t value) {
  StoreImpl<int16_t>(addr, order, value);
}

extern "C" void __ltest_wmm_store_i32(void* addr, int order, int32_t value) {
  StoreImpl<int32_t>(addr, order, value);
}

extern "C" void __ltest_wmm_store_i64(void* addr, int order, int64_t value) {
  StoreImpl<int64_t>(addr, order, value);
}


extern "C" LTestCmpXchgResult_i1 __ltest_wmm_cmpxchg_i1(
    void* addr,
    bool expected,
    bool desired,
    int success_order,
    int failure_order,
    bool is_weak) {
  auto result = CmpXchgImpl<bool>(
      addr,
      expected,
      desired,
      success_order,
      failure_order,
      is_weak);
  return {result.old, result.success};
}

extern "C" LTestCmpXchgResult_i8 __ltest_wmm_cmpxchg_i8(
    void* addr,
    int8_t expected,
    int8_t desired,
    int success_order,
    int failure_order,
    bool is_weak) {
  auto result = CmpXchgImpl<int8_t>(
      addr,
      expected,
      desired,
      success_order,
      failure_order,
      is_weak);
  return {result.old, result.success};
}

extern "C" LTestCmpXchgResult_i16 __ltest_wmm_cmpxchg_i16(
    void* addr,
    int16_t expected,
    int16_t desired,
    int success_order,
    int failure_order,
    bool is_weak) {
  auto result = CmpXchgImpl<int16_t>(
      addr,
      expected,
      desired,
      success_order,
      failure_order,
      is_weak);
  return {result.old, result.success};
}

extern "C" LTestCmpXchgResult_i32 __ltest_wmm_cmpxchg_i32(
    void* addr,
    int32_t expected,
    int32_t desired,
    int success_order,
    int failure_order,
    bool is_weak) {
  auto result = CmpXchgImpl<int32_t>(
      addr,
      expected,
      desired,
      success_order,
      failure_order,
      is_weak);
  return {result.old, result.success};
}

extern "C" LTestCmpXchgResult_i64 __ltest_wmm_cmpxchg_i64(
    void* addr,
    int64_t expected,
    int64_t desired,
    int success_order,
    int failure_order,
    bool is_weak) {
  auto result = CmpXchgImpl<int64_t>(
      addr,
      expected,
      desired,
      success_order,
      failure_order,
      is_weak);
  return {result.old, result.success};
}


extern "C" int8_t __ltest_wmm_rmw_i8(
    void* addr, int order, int op, int8_t operand) {
  return AtomicRmwImpl<int8_t>(addr, order, op, operand);
}

extern "C" int16_t __ltest_wmm_rmw_i16(
    void* addr, int order, int op, int16_t operand) {
  return AtomicRmwImpl<int16_t>(addr, order, op, operand);
}

extern "C" int32_t __ltest_wmm_rmw_i32(
    void* addr, int order, int op, int32_t operand) {
  return AtomicRmwImpl<int32_t>(addr, order, op, operand);
}

extern "C" int64_t __ltest_wmm_rmw_i64(
    void* addr, int order, int op, int64_t operand) {
  return AtomicRmwImpl<int64_t>(addr, order, op, operand);
}

extern "C" int __ltest_wmm_choice(int candidates_count) {
  // this is the only place where we give choice to the strategy
  auto* strategy = GetActiveStrategy();
  if (!strategy || candidates_count <= 0) {
    return 0;
  }
  if (candidates_count == 1) {
    return 0;
  }
  return static_cast<int>(strategy->ChooseCandidate(
      static_cast<size_t>(candidates_count)));
}
