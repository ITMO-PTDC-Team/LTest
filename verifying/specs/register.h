#include <cassert>
#include <functional>
#include <map>
#include <string>

namespace spec {

struct LinearRegister;

using mutex_method_t = std::function<int(LinearRegister *l, void *)>;

struct LinearRegister {
  int x = 0;
  int add() {
    ++x;
    return 0;
  }
  int get() { return x; }

  static auto GetMethods() {
    mutex_method_t add_func = [](LinearRegister *l, void *) -> int {
      return l->add();
    };

    mutex_method_t get_func = [](LinearRegister *l, void *) -> int {
      return l->get();
    };

    return std::map<std::string, mutex_method_t>{
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
