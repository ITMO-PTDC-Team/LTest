#include <atomic>
#include <condition_variable>
#include <cstring>
#include <mutex>

#include "../specs/queue.h"
#include "runtime/include/value_wrapper.h"
#include "runtime/include/verifying.h"
#include "verifiers/buffered_channel_verifier.h"

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

struct BufferedChannel {
  non_atomic void Send(int v) {
    std::unique_lock lock{mutex_};
    while (!closed_ && full_) {
      debug(stderr, "Waiting in send...\n");
      send_side_cv_.wait(lock);
    }
    debug(stderr, "Send\n");

    queue_[sidx_] = v;
    sidx_ = (sidx_ + 1) % N;
    full_ = (sidx_ == ridx_);
    empty_ = false;
    recv_side_cv_.notify_one();
  }

  non_atomic int Recv() {
    std::unique_lock lock{mutex_};
    while (!closed_ && empty_) {
      debug(stderr, "Waiting in recv...\n");
      recv_side_cv_.wait(lock);
    }
    debug(stderr, "Recv\n");
    auto val = queue_[ridx_];
    ridx_ = (ridx_ + 1) % 5;
    empty_ = (sidx_ == ridx_);
    full_ = false;
    send_side_cv_.notify_one();
    return val;
  }

  void Close() {
    closed_.store(true);
    send_side_cv_.notify_all();
    recv_side_cv_.notify_all();
  }

  std::mutex mutex_;
  std::condition_variable send_side_cv_, recv_side_cv_;
  std::atomic<bool> closed_{false};

  bool full_{false};
  bool empty_{true};

  uint32_t sidx_{0}, ridx_{0};

  std::array<int, N> queue_{};
};

auto generateInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

using spec_t =
    ltest::Spec<BufferedChannel, spec::BufferedChannel,
                spec::BufferedChannelHash, spec::BufferedChannelEquals>;

LTEST_ENTRYPOINT_CONSTRAINT(spec_t, spec::BufferedChannelVerifier);

target_method(generateInt, void, BufferedChannel, Send, int);
target_method(ltest::generators::genEmpty, int, BufferedChannel, Recv);
