#pragma once

#include <any>
#include <functional>
#include <string>
class ValueWrapper;

using to_string_func = std::function<std::string(const ValueWrapper&)>;
using comp_func = std::function<bool(const ValueWrapper&, const ValueWrapper&)>;

template <typename T>
to_string_func get_default_to_string();
template <typename T>
comp_func get_default_compator();

class ValueWrapper {
  std::any value_;
  comp_func compare_;
  to_string_func to_str_;

 public:
  ValueWrapper() = default;

  template <typename T>
  ValueWrapper(const T& t, const comp_func& cmp = get_default_compator<T>(),
               const to_string_func& str = get_default_to_string<T>())
      : value_(t), compare_(cmp), to_str_(str) {}
  bool operator==(const ValueWrapper& other) const {
    return compare_(*this, other);
  }
  friend std::string to_string(const ValueWrapper& wrapper) {
    return wrapper.to_str_(wrapper);
  }
  bool HasValue() const { return value_.has_value(); }
  template <typename T>
  T GetValue() const {
    return std::any_cast<T>(value_);
  }
};
template <typename T>
to_string_func get_default_to_string() {
  using std::to_string;
  return [](const ValueWrapper& a) { return to_string(a.GetValue<T>()); };
}

template <typename T>
comp_func get_default_compator() {
  return [](const ValueWrapper& a, const ValueWrapper& b) {
    if (a.HasValue() != b.HasValue()) {
      return false;
    }
    if ((!a.HasValue() && !b.HasValue())) {
      return true;
    }
    auto l = a.GetValue<T>();
    auto r = b.GetValue<T>();
    return l == r;
  };
}

class Void {};

static ValueWrapper void_v{Void{}, [](auto& a, auto& b) { return true; },
                           [](auto& a) { return "void"; }};