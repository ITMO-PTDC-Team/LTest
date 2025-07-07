#include "../specs/buffered_channel.h"

#include <atomic>
#include <condition_variable>
#include <cstring>
#include <mutex>

#include "runtime/include/verifying.h"
#include "verifiers/buffered_channel_verifier.h"

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
    ridx_ = (ridx_ + 1) % N;
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
