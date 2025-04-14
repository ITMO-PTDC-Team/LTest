#include <set>

#include "../../runtime/include/verifying.h"

namespace spec {

template <typename ArgTuple = std::tuple<int>, std::size_t ValueIndex = 0>
struct Set {
  std::set<int> st{};
  int Insert(int v) {
    auto [_, added] = st.insert(v);
    return added;
  }

  int Erase(int v) {
    int how_many = st.erase(v);
    return how_many != 0;
  }

  using method_t = std::function<ValueWrapper(Set *l, void *args)>;
  static auto GetMethods() {
    method_t insert_func = [](Set *l, void *args) -> int {
      auto real_args = reinterpret_cast<ArgTuple *>(args);
      return l->Insert(std::get<ValueIndex>(*real_args));
    };

    method_t erase_func = [](Set *l, void *args) -> int {
      auto real_args = reinterpret_cast<ArgTuple *>(args);
      return l->Erase(std::get<ValueIndex>(*real_args));
    };

    return std::map<std::string, method_t>{
        {"Insert", insert_func},
        {"Erase", erase_func},
    };
  }
};

template <typename SetCls = Set<>>
struct SetHash {
  size_t operator()(const SetCls &r) const {
    int res = 0;
    for (int elem : r.st) {
      res += elem;
    }
    return res;
  }  // namespace spec
};

template <typename SetCls = Set<>>
struct SetEquals {
  template <typename PushArgTuple, int ValueIndex>
  bool operator()(const SetCls &lhs, const SetCls &rhs) const {
    return lhs.st == rhs.st;
  }
};

}  // namespace spec
