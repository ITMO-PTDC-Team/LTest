// Keeps as separated file because use in regression tests.
#pragma once
#include <cassert>
#include <vector>

#include "generators.h"
#include "lib.h"

namespace ltest {

extern std::vector<TaskBuilder> task_builders;

}  // namespace ltest

// Adds an attribute.
#define attr(attr) __attribute((__annotate__(#attr)))

// Tell that the function need to be converted to the coroutine.
#define non_atomic attr(ltest_nonatomic)

namespace ltest {

template <typename T>
std::string toString(const T &a);

template <typename T>
std::string toString(const T &a) requires (std::is_integral_v<T>){
  return std::to_string(a);
}

template <typename tuple_t, size_t... index>
auto toStringListHelper(const tuple_t &t,
                        std::index_sequence<index...>) noexcept {
  return std::vector<std::string>{ltest::toString(std::get<index>(t))...};
}

template <typename tuple_t>
auto toStringList(const tuple_t &t) noexcept {
  typedef typename std::remove_reference<decltype(t)>::type tuple_type;
  constexpr auto s = std::tuple_size<tuple_type>::value;
  if constexpr (s == 0) {
    return std::vector<std::string>{};
  }
  return toStringListHelper<tuple_type>(t, std::make_index_sequence<s>{});
}

template <typename... Args>
auto toStringArgs(std::shared_ptr<void> args) {
  auto real_args = reinterpret_cast<std::tuple<Args...> *>(args.get());
  return toStringList(*real_args);
}

template <typename Ret, typename Target, typename... Args>
struct TargetMethod {
  using Method = std::function<value_wrapper(Target *, Args...)>;
  TargetMethod(std::string_view method_name,
               std::function<std::tuple<Args...>(size_t)> gen, Method method) {
    auto builder = [gen = std::move(gen), method_name,
                    method = std::move(method)](void *this_ptr,
                                                size_t thread_num) -> Task {
      auto args = std::shared_ptr<void>(new std::tuple(gen(thread_num)));
      auto coro = Coro<Target, Args...>::New(
          method, this_ptr, args, &ltest::toStringArgs<Args...>, method_name);
      if (ltest::generators::generated_token) {
        coro->SetToken(ltest::generators::generated_token);
        ltest::generators::generated_token.reset();
      }
      return coro;
    };
    ltest::task_builders.push_back(
        TaskBuilder(std::string(method_name), builder));
  }
};

template <typename Target, typename... Args>
struct TargetMethod<void, Target, Args...> {
  using Method = std::function<void(Target *, Args...)>;

  TargetMethod(std::string_view method_name,
               std::function<std::tuple<Args...>(size_t)> gen, Method method) {
    auto builder = [gen = std::move(gen), method_name,
                    method = std::move(method)](void *this_ptr,
                                                size_t thread_num) -> Task {
      auto wrapper = [f = std::move(method)](void *this_ptr, Args &&...args) {
        f(reinterpret_cast<Target *>(this_ptr), std::forward<Args>(args)...);
        return VoidV;
      };
      auto args = std::shared_ptr<void>(new std::tuple(gen(thread_num)));
      auto coro = Coro<Target, Args...>::New(
          wrapper, this_ptr, args, &ltest::toStringArgs<Args...>, method_name);
      if (ltest::generators::generated_token) {
        coro->SetToken(ltest::generators::generated_token);
        ltest::generators::generated_token.reset();
      }
      return coro;
    };
    ltest::task_builders.push_back(
        TaskBuilder(std::string(method_name), builder));
  }
};

}  // namespace ltest

#define declare_task_name(symbol) const char *symbol##_task_name = #symbol

#define target_method(gen, ret, cls, symbol, ...)          \
  declare_task_name(symbol);                               \
  ltest::TargetMethod<ret, cls __VA_OPT__(, ) __VA_ARGS__> \
      symbol##_ltest_method_cls{symbol##_task_name, gen, &cls::symbol};
