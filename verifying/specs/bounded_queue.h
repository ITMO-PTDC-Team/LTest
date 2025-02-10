#include <cassert>
#include <deque>
#include <functional>
#include <map>
#include <tuple>

#include "../../runtime/include/verifying.h"

namespace spec {

struct Queue;

using method_t = std::function<value_wrapper(Queue *l, void *)>;

const int size = 2;

struct Queue {
  std::deque<int> deq{};

  int Push(int v) {
    if (deq.size() >= size) {
      return 0;
    }
    deq.push_back(v);
    return 1;
  }
  int Pop() {
    if (deq.empty()) return 0;
    int res = deq.front();
    deq.pop_front();
    return res;
  }
  static auto GetMethods() {
    method_t push_func = [](Queue *l, void *args) -> int {
      auto real_args = reinterpret_cast<std::tuple<int> *>(args);
      return l->Push(std::get<0>(*real_args));
    };

    method_t pop_func = [](Queue *l, void *args) -> int { return l->Pop(); };

    return std::map<std::string, method_t>{
        {"Push", push_func},
        {"Pop", pop_func},
    };
  }
};

struct QueueHash {
  size_t operator()(const Queue &r) const {
    int res = 0;
    for (int elem : r.deq) {
      res += elem;
    }
    return res;
  }
};

struct QueueEquals {
  bool operator()(const Queue &lhs, const Queue &rhs) const {
    return lhs.deq == rhs.deq;
  }
};

}  // namespace spec
