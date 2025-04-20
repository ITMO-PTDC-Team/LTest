#include <cassert>
#include <functional>
#include <map>
#include <string>
#include "runtime/include/value_wrapper.h"

namespace spec {

struct LinearRegister;

using MutexMethodT = std::function<ValueWrapper(LinearRegister *l, void *)>;

struct LinearRegister {
  int x = 0;
  void Add() { ++x; }
  int Get() { return x; }

  static auto GetMethods() {
    MutexMethodT add_func = [](LinearRegister *l, void *) {
      l->Add();
      return void_v;
    };

    MutexMethodT get_func = [](LinearRegister *l, void *)  {
      return l->Get();
    };

    return std::map<std::string, MutexMethodT>{
        {"add", add_func},
        {"get", get_func},
    };
  }
};

struct LinearRegisterHash {
  size_t operator()(const LinearRegister &r) const { return r.x; }
};

struct LinearRegisterEquals {
  bool operator()(const LinearRegister &lhs, const LinearRegister &rhs) const {
    return lhs.x == rhs.x;
  }
};

}  // namespace spec
