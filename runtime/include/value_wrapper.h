#pragma once

#include <any>
#include <functional>
#include <string>
class value_wrapper;

using to_string_func = std::function<std::string(const value_wrapper&)>;
using comp_func =
    std::function<bool(const value_wrapper&, const value_wrapper&)>;

namespace value_wrapper_details {
template <typename T>
to_string_func get_default_to_string();
template <typename T>
comp_func get_default_compator();
}  // namespace value_wrapper_details
class value_wrapper {
  std::any value;
  comp_func compare;
  to_string_func toStr;

 public:
  value_wrapper() = default;

  template <typename T>
  value_wrapper(
      const T& t,
      const comp_func& cmp = value_wrapper_details::get_default_compator<T>(),
      const to_string_func& str =
          value_wrapper_details::get_default_to_string<T>())
      : value(t), compare(cmp), toStr(str) {}
  bool operator==(const value_wrapper& other) const {
    return compare(*this, other);
  }
  friend std::string to_string(const value_wrapper& wrapper) {
    return wrapper.toStr(wrapper);
  }
  bool has_value() const { return value.has_value(); }
  template <typename T>
  T get_value() const {
    return std::any_cast<T>(value);
  }
};
namespace value_wrapper_details {
template <typename T>
to_string_func get_default_to_string() {
  using std::to_string;
  return [](const value_wrapper& a) { return to_string(a.get_value<T>()); };
}

template <typename T>
comp_func get_default_compator() {
  return [](const value_wrapper& a, const value_wrapper& b) {
    if (a.has_value() != b.has_value()) {
      return false;
    }
    if ((!a.has_value() && !b.has_value())) {
      return true;
    }
    return a.get_value<T>() == b.get_value<T>();
  };
}

class Void {};
}  // namespace value_wrapper_details
static value_wrapper VoidV{value_wrapper_details::Void{},
                           [](auto& a, auto& b) { return true; },
                           [](auto& a) { return "void"; }};