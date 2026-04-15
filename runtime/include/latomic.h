#pragma once

#include <atomic>

#include "lib.h"
#include "wmm/wmm.h"

namespace ltest {

using namespace wmm;

// This class is intended to be the entry point
// for the weak memory logic later.
template <class T>
class latomic {
  std::atomic<T> atomicValue;
  int locationId = -1;
  ExecutionGraph& wmmGraph = ExecutionGraph::getInstance();

 private:
  bool IsWmmEnabled() const noexcept {
    return wmm_enabled && this_coro && locationId >= 0 && this_thread_id >= 0;
  }

 public:
#if __cplusplus >= 201703L  // C++17
  static constexpr bool is_always_lock_free =
      std::atomic<T>::is_always_lock_free;
#endif

  // Constructors
  constexpr latomic() noexcept : latomic(T{}) {}
  constexpr latomic(T desired) noexcept : atomicValue(desired) {
    if (wmm_enabled) {
      locationId = wmmGraph.RegisterLocation(desired);
    }
  }
  latomic(const latomic&) = delete;
  latomic& operator=(const latomic&) = delete;
  latomic& operator=(const latomic&) volatile = delete;

  // operator=
  T operator=(T desired) noexcept {
    store(desired);
    return desired;
  }

  T operator=(T desired) volatile noexcept {
    store(desired);
    return desired;
  }

  // is_lock_free
  bool is_lock_free() const noexcept { return atomicValue.is_lock_free(); }

  bool is_lock_free() const volatile noexcept {
    return atomicValue.is_lock_free();
  }

  // store
  void store(T desired,
             std::memory_order order = std::memory_order_seq_cst) noexcept {
    atomicValue.store(desired, order);

    if (IsWmmEnabled()) {
      // std::cout << "Store: coro id=" << this_coro->GetId() << ", thread=" <<
      // this_thread_id
      //           << ", name=" << this_coro->GetName() << std::endl;
      wmmGraph.Store(locationId, this_thread_id, WmmUtils::OrderFromStd(order),
                     desired);
    }
  }

  void store(T desired, std::memory_order order =
                            std::memory_order_seq_cst) volatile noexcept {
    const_cast<latomic*>(this)->store(desired, order);
  }

  // load
  T load(std::memory_order order = std::memory_order_seq_cst) const noexcept {
    if (IsWmmEnabled()) {
      // std::cout << "Load: coro id=" << this_coro->GetId() << ", thread=" <<
      // this_thread_id
      //           << ", name=" << this_coro->GetName() << std::endl;
      auto result = wmmGraph.Load<T>(locationId, this_thread_id,
                                     WmmUtils::OrderFromStd(order));

      if (result.has_value()) {
        return result.value();
      }
    }

    return atomicValue.load(order);
  }

  T load(std::memory_order order = std::memory_order_seq_cst) const
      volatile noexcept {
    return const_cast<latomic*>(this)->load(order);
  }

  // operator T()
  operator T() const noexcept { return load(); }

  operator T() const volatile noexcept { return load(); }

  // exchange
  T exchange(T desired,
             std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.exchange(desired, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::Exchange, desired,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T exchange(T desired, std::memory_order order =
                            std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->exchange(desired, order);
  }

  // TODO: for CASes there more complex rule how to get 'failure' order from
  // 'success', implement them instead of blind defaults compare_exchange_weak
  bool compare_exchange_weak(T& expected, T desired, std::memory_order success,
                             std::memory_order failure) noexcept {
    // we want to prevent actual atomics from overriding 'expected' value on rmw
    // failure
    T myExpected = expected;
    bool value = atomicValue.compare_exchange_weak(myExpected, desired, success,
                                                   failure);

    if (IsWmmEnabled()) {
      // std::cout << "Compare exchange weak: coro id=" << this_coro->GetId() <<
      // ", thread=" << this_thread_id
      //           << ", name=" << this_coro->GetName() << std::endl;
      auto rmwResult = wmmGraph.ReadModifyWrite(
          locationId, this_thread_id, &expected, desired,
          WmmUtils::OrderFromStd(success), WmmUtils::OrderFromStd(failure));
      if (rmwResult.has_value()) {
        auto [rmwSuccess, readValue] = rmwResult.value();
        value = rmwSuccess;
      }
    } else {
      // update expected only if we are not in a coroutine
      expected = myExpected;
    }

    return value;
  }

  bool compare_exchange_weak(T& expected, T desired, std::memory_order success,
                             std::memory_order failure) volatile noexcept {
    return const_cast<latomic*>(this)->compare_exchange_weak(expected, desired,
                                                             success, failure);
  }

  bool compare_exchange_weak(
      T& expected, T desired,
      std::memory_order order = std::memory_order_seq_cst) noexcept {
    return compare_exchange_weak(expected, desired, order,
                                 std::memory_order_seq_cst);
  }

  bool compare_exchange_weak(
      T& expected, T desired,
      std::memory_order order = std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->compare_exchange_weak(
        expected, desired, order, std::memory_order_seq_cst);
  }

  // compare_exchange_strong
  bool compare_exchange_strong(T& expected, T desired,
                               std::memory_order success,
                               std::memory_order failure) noexcept {
    // we want to prevent actual atomics from overriding 'expected' value on rmw
    // failure
    T myExpected = expected;
    bool value = atomicValue.compare_exchange_strong(myExpected, desired,
                                                     success, failure);

    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.ReadModifyWrite(
          locationId, this_thread_id, &expected, desired,
          WmmUtils::OrderFromStd(success), WmmUtils::OrderFromStd(failure));
      if (rmwResult.has_value()) {
        auto [rmwSuccess, readValue] = rmwResult.value();
        value = rmwSuccess;
      }
    } else {
      // update expected only if we are not in a coroutine
      expected = myExpected;
    }

    return value;
  }

  bool compare_exchange_strong(T& expected, T desired,
                               std::memory_order success,
                               std::memory_order failure) volatile noexcept {
    return const_cast<latomic*>(this)->compare_exchange_strong(
        expected, desired, success, failure);
  }

  bool compare_exchange_strong(
      T& expected, T desired,
      std::memory_order order = std::memory_order_seq_cst) noexcept {
    return compare_exchange_strong(expected, desired, order,
                                   std::memory_order_seq_cst);
  }

  bool compare_exchange_strong(
      T& expected, T desired,
      std::memory_order order = std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->compare_exchange_strong(
        expected, desired, order, std::memory_order_seq_cst);
  }

  // Note: wmm support for wait/notify methods are out of scope for now. Tests
  // which use them are not supported.
// wait
#if __cplusplus >= 202002L  // C++20
  void wait(T old, std::memory_order order =
                       std::memory_order_seq_cst) const noexcept {
    atomicValue.wait(old, order);
  }

  void wait(T old, std::memory_order order = std::memory_order_seq_cst) const
      volatile noexcept {
    atomicValue.wait(old, order);
  }

  // notify_one
  void notify_one() noexcept { atomicValue.notify_one(); }

  void notify_one() volatile noexcept { atomicValue.notify_one(); }

  // notify all
  void notify_all() noexcept { atomicValue.notify_all(); }

  void notify_all() volatile noexcept { atomicValue.notify_all(); }
#endif

  // fetch_add
  T fetch_add(T arg,
              std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_add(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchAdd, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_add(T arg, std::memory_order order =
                         std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_add(arg, order);
  }

  // TODO: fix ambiguity with specialization for T*
  // T* fetch_add(std::ptrdiff_t arg, std::memory_order order =
  // std::memory_order_seq_cst) noexcept {
  //   return atomicValue.fetch_add(arg, order);
  // }

  // T* fetch_add(std::ptrdiff_t arg, std::memory_order order =
  // std::memory_order_seq_cst) volatile noexcept {
  //   return atomicValue.fetch_add(arg, order);
  // }

  // fetch_sub
  T fetch_sub(T arg,
              std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_sub(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchSub, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_sub(T arg, std::memory_order order =
                         std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_sub(arg, order);
  }

  // TODO: fix ambiguity with specialization for T*
  // T* fetch_sub(std::ptrdiff_t arg, std::memory_order order =
  // std::memory_order_seq_cst) noexcept {
  //   return atomicValue.fetch_sub(arg, order);
  // }

  // T* fetch_sub(std::ptrdiff_t arg, std::memory_order order =
  // std::memory_order_seq_cst) volatile noexcept {
  //   return atomicValue.fetch_sub(arg, order);
  // }

  // operator+=
  T operator+=(T arg) noexcept { return fetch_add(arg) + arg; }

  T operator+=(T arg) volatile noexcept {
    return const_cast<latomic*>(this)->operator+=(arg);
  }

  // TODO: fix ambiguity with specialization for T*
  // T* operator+=(std::ptrdiff_t arg) noexcept {
  //   return atomicValue.operator+=(arg);
  // }

  // T* operator+=(std::ptrdiff_t arg) volatile noexcept {
  //   return atomicValue.operator+=(arg);
  // }

  // operator-=
  T operator-=(T arg) noexcept { return fetch_sub(arg) - arg; }

  T operator-=(T arg) volatile noexcept {
    return const_cast<latomic*>(this)->operator-=(arg);
  }

  // TODO: fix ambiguity with specialization for T*
  // T* operator-=(std::ptrdiff_t arg) noexcept {
  //   return atomicValue.operator-=(arg);
  // }

  // T* operator-=(std::ptrdiff_t arg) volatile noexcept {
  //   return atomicValue.operator-=(arg);
  // }

  // fetch_max
  T fetch_max(T arg,
              std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_max(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchMax, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_max(T arg, std::memory_order order =
                         std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_max(arg, order);
  }

  // fetch_min
  T fetch_min(T arg,
              std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_min(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchMin, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_min(T arg, std::memory_order order =
                         std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_min(arg, order);
  }

  // operator++
  T operator++() noexcept { return fetch_add(1) + 1; }

  T operator++() volatile noexcept {
    return const_cast<latomic*>(this)->operator++();
  }

  T operator++(int) noexcept { return fetch_add(1); }

  T operator++(int) volatile noexcept {
    return const_cast<latomic*>(this)->operator++(0);
  }

  // operator--
  T operator--() noexcept { return fetch_sub(1) - 1; }

  T operator--() volatile noexcept {
    return const_cast<latomic*>(this)->operator--();
  }

  T operator--(int) noexcept { return fetch_sub(1); }

  T operator--(int) volatile noexcept {
    return const_cast<latomic*>(this)->operator--(0);
  }

  // fetch_and
  T fetch_and(T arg,
              std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_and(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchAnd, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_and(T arg, std::memory_order order =
                         std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_and(arg, order);
  }

  // fetch_or
  T fetch_or(T arg,
             std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_or(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchOr, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_or(T arg, std::memory_order order =
                        std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_or(arg, order);
  }

  // fetch_xor
  T fetch_xor(T arg,
              std::memory_order order = std::memory_order_seq_cst) noexcept {
    T prev = atomicValue.fetch_xor(arg, order);
    if (IsWmmEnabled()) {
      auto rmwResult = wmmGraph.UnconditionalReadModifyWrite<T>(
          locationId, this_thread_id, AtomicRmwOp::FetchXor, arg,
          WmmUtils::OrderFromStd(order));

      if (rmwResult.has_value()) {
        return rmwResult.value();
      }
    }
    return prev;
  }

  T fetch_xor(T arg, std::memory_order order =
                         std::memory_order_seq_cst) volatile noexcept {
    return const_cast<latomic*>(this)->fetch_xor(arg, order);
  }

  // operator&=
  T operator&=(T arg) noexcept { return fetch_and(arg) & arg; }

  T operator&=(T arg) volatile noexcept {
    return const_cast<latomic*>(this)->operator&=(arg);
  }

  // operator|=
  T operator|=(T arg) noexcept { return fetch_or(arg) | arg; }

  T operator|=(T arg) volatile noexcept {
    return const_cast<latomic*>(this)->operator|=(arg);
  }

  // operator^=
  T operator^=(T arg) noexcept { return fetch_xor(arg) ^ arg; }

  T operator^=(T arg) volatile noexcept {
    return const_cast<latomic*>(this)->operator^=(arg);
  }
};

}  // namespace ltest