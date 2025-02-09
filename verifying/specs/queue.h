#include <cassert>
#include <deque>
#include <functional>
#include <map>

#include "../../runtime/include/verifying.h"

namespace spec {

template <typename PushArgTuple = std::tuple<int>, std::size_t ValueIndex = 0>
struct Queue {
  std::deque<int> deq{};
  int Push(int v) {
    deq.push_back(v);
    return 0;
  }
  int Pop() {
    if (deq.empty()) return 0;
    int res = deq.front();
    deq.pop_front();
    return res;
  }

  using method_t = std::function<value_wrapper(Queue *l, void *args)>;
  static auto GetMethods() {
    method_t push_func = [](Queue *l, void *args) -> int {
      auto real_args = reinterpret_cast<PushArgTuple *>(args);
      return l->Push(std::get<ValueIndex>(*real_args));
    };

    method_t pop_func = [](Queue *l, void *args) -> int { return l->Pop(); };

    return std::map<std::string, method_t>{
        {"Push", push_func},
        {"Pop", pop_func},
    };
  }
};

template <typename QueueCls = Queue<>>
struct QueueHash {
  size_t operator()(const QueueCls &r) const {
    int res = 0;
    for (int elem : r.deq) {
      res += elem;
    }
    return res;
  }  // namespace spec
};

template <typename QueueCls = Queue<>>
struct QueueEquals {
  template <typename PushArgTuple, int ValueIndex>
  bool operator()(const QueueCls &lhs, const QueueCls &rhs) const {
    return lhs.deq == rhs.deq;
  }
};

}  // namespace spec
