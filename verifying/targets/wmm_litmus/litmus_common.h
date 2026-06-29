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
  using method_t = std::function<ValueWrapper(LitmusTwoThreadsSpec *, void *)>;
  static auto GetMethods() {
    method_t func = [](LitmusTwoThreadsSpec *, void *) -> ValueWrapper {
      return void_v;
    };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
    };
  }
};

struct LitmusThreeThreadsSpec {
  using method_t =
      std::function<ValueWrapper(LitmusThreeThreadsSpec *, void *)>;
  static auto GetMethods() {
    method_t func = [](LitmusThreeThreadsSpec *, void *) -> ValueWrapper {
      return void_v;
    };
    return std::map<std::string, method_t>{
        {"A", func},
        {"B", func},
        {"C", func},
    };
  }
};