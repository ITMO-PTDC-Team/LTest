#include <optional>
#include <queue>
#include <stack>
#include <string>
#include <utility>

#include "../../runtime/include/verifying.h"
#include "support_coro.h"

constexpr int writer_count = 2;
constexpr int start_id = 100;
namespace spec {

struct CommunicationRef {
  std::deque<int> buf;
  int id = start_id;
  CommunicationRef() {}
  CommunicationRef &operator=(const CommunicationRef &oth) { return *this; }

  void Send(int i) {
    buf.push_back(id);
    id++;
    buf.push_back(i);
  }

  void Receive() {
    int m_id = buf.front();
    buf.pop_front();
    int message = buf.front();
    buf.pop_front();
  }
  using MethodT = std::function<ValueWrapper(CommunicationRef *l, void *args)>;
  static auto GetMethods() {
    MethodT receive = [](CommunicationRef *l, void *args) {
      l->Receive();
      return void_v;
    };
    MethodT send = [](CommunicationRef *l, void *args) {
      auto real_args = reinterpret_cast<std::tuple<int> *>(args);
      l->Send(std::get<0>(*real_args));
      return void_v;
    };
    return std::map<std::string, MethodT>{{"Send", send}, {"Receive", receive}};
  }
};

struct UniqueArgsHash {
  size_t operator()(const CommunicationRef &r) const {
    int res = 0;
    for (int elem : r.buf) {
      res += elem;
    }
    return res;
  }
};
struct UniqueArgsEquals {
  bool operator()(const CommunicationRef &lhs,
                  const CommunicationRef &rhs) const {
    return lhs.buf == rhs.buf;
  }
};
struct UniqueArgsOptionsOverride {
  static ltest::DefaultOptions GetOptions() {
    return {.threads = writer_count + 1,
            .tasks = writer_count + 1,
            .switches = 100000000,
            .rounds = 10000,
            .depth = 1,
            .forbid_all_same = false,
            .verbose = false,
            .strategy = "tla",
            .weights = ""};
  }
};

}  // namespace spec
