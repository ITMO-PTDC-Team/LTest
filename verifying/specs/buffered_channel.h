
#include <deque>
#include <functional>
#include <map>

#include "runtime/include/value_wrapper.h"

constexpr int N = 5;

namespace spec {
struct BufferedChannel {
  void Send(int v) { deq.push_back(v); }

  int Recv() {
    if (deq.empty()) {
      return -1;
    }
    auto value = deq.front();
    deq.pop_front();
    return value;
  }

  using method_t = std::function<ValueWrapper(BufferedChannel *l, void *args)>;
  static auto GetMethods() {
    method_t send_func = [](BufferedChannel *l, void *args) {
      auto real_args = reinterpret_cast<std::tuple<int> *>(args);
      l->Send(std::get<0>(*real_args));
      return void_v;
    };

    method_t recv_func = [](BufferedChannel *l, void *args) -> int {
      return l->Recv();
    };

    return std::map<std::string, method_t>{
        {"Send", send_func},
        {"Recv", recv_func},
    };
  }

  std::deque<int> deq;
};

struct BufferedChannelHash {
  size_t operator()(const BufferedChannel &r) const {
    int res = 0;
    for (int elem : r.deq) {
      res += elem;
    }
    return res;
  }
};

struct BufferedChannelEquals {
  bool operator()(const BufferedChannel &lhs,
                  const BufferedChannel &rhs) const {
    return lhs.deq == rhs.deq;
  }
};
};  // namespace spec