#include "../specs/coro_async_stack.h"

#include <atomic>
#include <coroutine>
#include <cstdlib>
#include <type_traits>
#include <utility>
#include <variant>

#include "../../runtime/include/verifying.h"

// ---------------------------
// Self-contained coroutine dual stack (ABA is possible).
// ---------------------------
namespace coro {

template <class T>
class async_stack;

namespace detail {

template <class T>
struct stack_node_base : std::variant<T, std::coroutine_handle<>> {
  using slot_t = std::variant<T, std::coroutine_handle<>>;

  stack_node_base(std::in_place_type_t<T>, T&& v)
      : slot_t(std::in_place_type<T>, std::move(v)) {}

  stack_node_base(std::in_place_type_t<std::coroutine_handle<>>,
                  std::coroutine_handle<> h)
      : slot_t(std::in_place_type<std::coroutine_handle<>>, h) {}

  stack_node_base* m_next{nullptr};
};

template <class T>
struct value_node final : stack_node_base<T> {
  explicit value_node(T v)
      : stack_node_base<T>(std::in_place_type<T>, std::move(v)) {}
};

template <class T>
static auto to_node(typename stack_node_base<T>::slot_t* p) noexcept
    -> stack_node_base<T>* {
  return reinterpret_cast<stack_node_base<T>*>(p);
}

template <class T>
static auto to_slot(stack_node_base<T>* n) noexcept
    -> typename stack_node_base<T>::slot_t* {
  return static_cast<typename stack_node_base<T>::slot_t*>(n);
}

template <class T>
struct pop_operation_base : stack_node_base<T> {
  explicit pop_operation_base(coro::async_stack<T>& s) noexcept
      : stack_node_base<T>(std::in_place_type<std::coroutine_handle<>>,
                           std::coroutine_handle<>{}),
        m_stack(s) {}

  // Marked non_atomic so YieldPass inserts CoroYield() inside these methods.
  non_atomic auto await_ready() noexcept -> bool;
  non_atomic auto await_suspend(std::coroutine_handle<> awaiting) noexcept
      -> bool;

 protected:
  coro::async_stack<T>& m_stack;
};

template <class T>
struct pop_operation final : pop_operation_base<T> {
  explicit pop_operation(coro::async_stack<T>& s) noexcept
      : pop_operation_base<T>(s) {}

  auto await_resume() noexcept -> T {
    std::atomic_thread_fence(std::memory_order::acquire);
    return std::move(std::get<T>(*this));
  }
};

}  // namespace detail

template <class T>
class async_stack {
 public:
  using slot_t = std::variant<T, std::coroutine_handle<>>;

  non_atomic void push(T v) {
    static_assert(std::is_nothrow_move_constructible_v<T>);

    detail::value_node<T>* my_node = nullptr;

    slot_t* cur = m_head.load(std::memory_order::acquire);
    for (;;) {
      if (cur != nullptr) {
        auto* top = detail::to_node<T>(cur);

        if (std::holds_alternative<std::coroutine_handle<>>(*top)) {
          slot_t* next = detail::to_slot<T>(top->m_next);

          if (m_head.compare_exchange_weak(cur, next,
                                          std::memory_order::acq_rel,
                                          std::memory_order::acquire)) {
            T value = my_node ? std::move(std::get<T>(*my_node)) : std::move(v);
            //delete my_node;

            auto h = std::get<std::coroutine_handle<>>(*top);
            top->template emplace<T>(std::move(value));

            std::atomic_thread_fence(std::memory_order::release);
            h.resume();
            return;
          }
          continue;
        }
      }

      if (my_node == nullptr) {
        my_node = new detail::value_node<T>(std::move(v));
      }

      my_node->m_next = (cur == nullptr) ? nullptr : detail::to_node<T>(cur);

      if (m_head.compare_exchange_weak(cur, detail::to_slot<T>(my_node),
                                       std::memory_order::acq_rel,
                                       std::memory_order::acquire)) {
        return;
      }
    }
  }

  [[nodiscard]] auto pop() noexcept -> detail::pop_operation<T> {
    return detail::pop_operation<T>{*this};
  }

 private:
  friend struct detail::pop_operation_base<T>;
  std::atomic<slot_t*> m_head{nullptr};
};

namespace detail {

template <class T>
non_atomic auto pop_operation_base<T>::await_ready() noexcept -> bool {
  using slot_t = typename stack_node_base<T>::slot_t;

  auto& head = m_stack.m_head;
  slot_t* cur = head.load(std::memory_order::acquire);

  for (;;) {
    if (cur == nullptr) return false;

    auto* top = detail::to_node<T>(cur);

    if (std::holds_alternative<std::coroutine_handle<>>(*top)) return false;

    slot_t* next = detail::to_slot<T>(top->m_next);
    if (head.compare_exchange_weak(cur, next,
                                   std::memory_order::acq_rel,
                                   std::memory_order::acquire)) {
      auto* heap_node = static_cast<detail::value_node<T>*>(top);
      this->template emplace<T>(std::move(std::get<T>(*heap_node)));
      //delete heap_node;
      return true;
    }
  }
}

template <class T>
non_atomic auto pop_operation_base<T>::await_suspend(
    std::coroutine_handle<> awaiting) noexcept -> bool {
  using slot_t = typename stack_node_base<T>::slot_t;

  this->template emplace<std::coroutine_handle<>>(awaiting);

  auto& head = m_stack.m_head;
  slot_t* cur = head.load(std::memory_order::acquire);

  for (;;) {
    if (cur != nullptr) {
      auto* top = detail::to_node<T>(cur);

      if (std::holds_alternative<T>(*top)) {
        slot_t* next = detail::to_slot<T>(top->m_next);

        if (head.compare_exchange_weak(cur, next,
                                       std::memory_order::acq_rel,
                                       std::memory_order::acquire)) {
          auto* heap_node = static_cast<detail::value_node<T>*>(top);
          this->template emplace<T>(std::move(std::get<T>(*heap_node)));
          //delete heap_node;
          return false;
        }
        continue;
      }
    }

    this->m_next = (cur == nullptr) ? nullptr : detail::to_node<T>(cur);

    if (head.compare_exchange_weak(cur, detail::to_slot<T>(this),
                                   std::memory_order::acq_rel,
                                   std::memory_order::acquire)) {
      return true;
    }
  }
}

}  // namespace detail

}  // namespace coro

// ---------------------------
// LTest target wrapper + registration
// ---------------------------
struct CoroAsyncStackTarget {
  coro::async_stack<int> st;

  non_atomic void push(int v) { st.push(v); }
  auto pop() noexcept { return st.pop(); }
};

static auto genInt(size_t /*thread_id*/) {
  return ltest::generators::makeSingleArg(std::rand() % 10);
}

target_method(genInt, void, CoroAsyncStackTarget, push, int);
target_method_dual(ltest::generators::genEmpty, int, CoroAsyncStackTarget, pop);

using spec_t = ltest::SpecDual<CoroAsyncStackTarget, spec::CoroAsyncStack,
                              spec::CoroAsyncStackHash,
                              spec::CoroAsyncStackEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);