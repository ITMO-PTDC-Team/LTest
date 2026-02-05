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
