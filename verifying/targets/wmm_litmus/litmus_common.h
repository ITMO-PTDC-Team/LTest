#pragma once

#include "runtime/include/verifying.h"
#include "runtime/include/verifying_macro.h"

struct LinearWmmHash {
  template <typename T>
  size_t operator()(const T &) const {
    return 1;
  }
};

struct LinearWmmEquals {
  template <typename T>
  bool operator()(const T &, const T &) const {
    return true;
  }
};

struct LitmusTwoThreadsSpec {
  using method_t = std::function<ltest::ValueWrapper(LitmusTwoThreadsSpec *, void *)>;
  static auto GetMethods() {
    method_t func = [](LitmusTwoThreadsSpec *, void *) -> ltest::ValueWrapper {
      return ltest::void_v;
    };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

struct LitmusThreeThreadsSpec {
  using method_t =
      std::function<ltest::ValueWrapper(LitmusThreeThreadsSpec *, void *)>;
  static auto GetMethods() {
    method_t func = [](LitmusThreeThreadsSpec *, void *) -> ltest::ValueWrapper {
      return ltest::void_v;
    };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
        {"C", func},
    };
  }
};