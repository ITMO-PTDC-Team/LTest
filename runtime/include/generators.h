#pragma once
#include <tuple>
#include <type_traits>

#include "lib.h"

namespace ltest {

namespace generators {

extern std::shared_ptr<Token> generated_token;

// Makes single argument from the value.
template <typename T>
auto MakeSingleArg(T&& arg) {
  using ArgType = typename std::remove_reference<T>::type;
  return std::tuple<ArgType>{std::forward<ArgType>(arg)};
}

std::tuple<> GenEmpty(size_t thread_num);

std::tuple<std::shared_ptr<Token>> GenToken(size_t thread_num);

}  // namespace generators

}  // namespace ltest
