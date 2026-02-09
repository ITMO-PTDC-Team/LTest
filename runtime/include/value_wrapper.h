#pragma once

#include <any>
#include <functional>
#include <optional>
#include <string>
#include <type_traits>

class ValueWrapper;

using ToStringFunc = std::function<std::string(const ValueWrapper&)>;
using CompFunc = std::function<bool(const ValueWrapper&, const ValueWrapper&)>;

template <typename T>
ToStringFunc GetDefaultToString();
template <typename T>
CompFunc GetDefaultCompator();

class ValueWrapper {
  std::any value;
  CompFunc compare;
  ToStringFunc to_str;

 public:
  ValueWrapper() = default;

  template <typename T>
  ValueWrapper(const T& t, const CompFunc& cmp = GetDefaultCompator<T>(),
               const ToStringFunc& str = GetDefaultToString<T>())
      : value(t), compare(cmp), to_str(str) {}
  bool operator==(const ValueWrapper& other) const {
    return compare(*this, other);
  }
  // using std::to_string
  friend std::string to_string(const ValueWrapper& wrapper) {  // NOLINT
    return wrapper.to_str(wrapper);
  }
  bool HasValue() const { return value.has_value(); }
  template <typename T>
  T GetValue() const {
    return std::any_cast<T>(value);
  }
};
template <typename T>
ToStringFunc GetDefaultToString() {
  return [](const ValueWrapper& a) -> std::string {
    if constexpr (std::is_same_v<T, std::nullopt_t>) {
      return "nullopt";
    } else {
      using std::to_string;
      return to_string(a.GetValue<T>());
    }
  };
}

template <typename T>
CompFunc GetDefaultCompator() {
  return [](const ValueWrapper& a, const ValueWrapper& b) {
    if constexpr (std::is_same_v<T, std::nullopt_t>) {
      return true;  // all nullopt values are equivalent
    } else {
      if (a.HasValue() != b.HasValue()) {
        return false;
      }
      if ((!a.HasValue() && !b.HasValue())) {
        return true;
      }
      auto l = a.GetValue<T>();
      auto r = b.GetValue<T>();
      return l == r;
    }
  };
}

class Void {};

static ValueWrapper void_v{Void{}, [](auto& a, auto& b) { return true; },
                           [](auto& a) { return "void"; }};