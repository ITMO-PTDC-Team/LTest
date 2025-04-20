#include <cassert>
#include <deque>
#include <functional>
#include <map>

#include "../../runtime/include/verifying.h"
#include "runtime/include/value_wrapper.h"

namespace spec {

template <typename PushArgTuple = std::tuple<int>, std::size_t ValueIndex = 0>
struct Stack {
  std::deque<int> deq{};
  void Push(int v) { deq.push_back(v); }
  int Pop() {
    if (deq.empty()) return 0;
    int res = deq.back();
    deq.pop_back();
    return res;
  }

  using MethodT = std::function<ValueWrapper(Stack *l, void *args)>;
  static auto GetMethods() {
    MethodT push_func = [](Stack *l, void *args) -> ValueWrapper {
      auto real_args = reinterpret_cast<PushArgTuple *>(args);
      l->Push(std::get<ValueIndex>(*real_args));
      return void_v;
    };

    MethodT pop_func = [](Stack *l, void *args) -> int { return l->Pop(); };

    return std::map<std::string, MethodT>{
        {"Push", push_func},
        {"Pop", pop_func},
    };
  }
};

template <typename StackCls = Stack<>>
struct StackHash {
  size_t operator()(const StackCls &r) const {
    int res = 0;
    for (int elem : r.deq) {
      res += elem;
    }
    return res;
  }  // namespace spec
};

template <typename StackCls = Stack<>>
struct StackEquals {
  template <typename PushArgTuple, int ValueIndex>
  bool operator()(const StackCls &lhs, const StackCls &rhs) const {
    return lhs.deq == rhs.deq;
  }
};

}  // namespace spec
