#include <cassert>
#include <deque>
#include <functional>
#include <map>
#include <string>

#include "../../runtime/include/verifying.h"
#include "runtime/include/value_wrapper.h"

namespace spec {

template <typename PushArgTuple = std::tuple<int>, std::size_t ValueIndex = 0>
struct Queue {
  std::deque<int> deq{};
  void Push(int v) { deq.push_back(v); }
  int Pop() {
    if (deq.empty()) return 0;
    int res = deq.front();
    deq.pop_front();
    return res;
  }

  using method_t = std::function<ValueWrapper(Queue *l, void *args)>;
  static auto GetMethods() {
    method_t push_func = [](Queue *l, void *args) -> ValueWrapper {
      auto real_args = reinterpret_cast<PushArgTuple *>(args);
      l->Push(std::get<ValueIndex>(*real_args));
      return void_v;
    };

    method_t pop_func = [](Queue *l, void *args) -> int { return l->Pop(); };

    return std::map<std::string, method_t>{
        {"Push", push_func},
        {"Pop", pop_func},
    };
  }
};

struct QueueHash {
  size_t operator()(const Queue<> &r) const {
    int res = 0;
    for (int elem : r.deq) {
      res += elem;
    }
    return res;
  }
};

struct QueueEquals {
  template <typename PushArgTuple, int ValueIndex>
  bool operator()(const Queue<> &lhs, const Queue<> &rhs) const {
    return lhs.deq == rhs.deq;
  }
};

}  // namespace spec
