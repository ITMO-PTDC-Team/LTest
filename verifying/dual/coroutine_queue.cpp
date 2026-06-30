#include "../specs/coroutine_queue.h"

#include <algorithm>
#include <coroutine>
#include <cstdint>
#include <list>
#include <memory>
#include <mutex>
#include <optional>

#include "../../runtime/include/verifying.h"

struct CoroutineQueue {
  struct SendPromise;
  struct ReceivePromise;

  std::list<ReceivePromise> receivers;
  std::list<SendPromise> senders;
  ltest::mutex mutex;
  std::uint64_t next_waiter_id{1};

  SendPromise send(int elem) { return SendPromise(elem, *this); }
  ReceivePromise receive() { return ReceivePromise(*this); }

  struct ReceivePromise {
    explicit ReceivePromise(CoroutineQueue& queue) : queue(queue) {
      elem = std::make_shared<std::optional<int>>(std::nullopt);
    }
    as_atomic bool await_ready() { return false; }

    non_atomic bool await_suspend(std::coroutine_handle<> h) {
      std::coroutine_handle<> to_resume{};
      bool suspend = false;
      {
        std::lock_guard<ltest::mutex> lock(queue.mutex);
        if (!queue.senders.empty()) {
          SendPromise send_req = queue.senders.front();
          queue.senders.pop_front();
          *elem = send_req.elem;
          to_resume = send_req.sender;
        } else {
          id = queue.next_waiter_id++;
          receiver = h;
          queue.receivers.push_back(*this);
          suspend = true;
        }
      }

      if (to_resume) to_resume.resume();
      return suspend;
    }

    as_atomic void unregister() {
      if (id == 0) return;
      std::lock_guard<ltest::mutex> lock(queue.mutex);
      auto it = std::find_if(
          queue.receivers.begin(), queue.receivers.end(),
          [this](const ReceivePromise& waiter) { return waiter.id == id; });
      if (it != queue.receivers.end()) {
        queue.receivers.erase(it);
      }
      id = 0;
    }

    as_atomic int await_resume() {
      assert(elem->has_value());
      return **elem;
    }

    std::uint64_t id{0};
    std::coroutine_handle<> receiver{};
    std::shared_ptr<std::optional<int>> elem;
    CoroutineQueue& queue;
  };

  struct SendPromise {
    SendPromise(int elem, CoroutineQueue& queue) : elem(elem), queue(queue) {}

    as_atomic bool await_ready() { return false; }

    non_atomic bool await_suspend(std::coroutine_handle<> h) {
      std::coroutine_handle<> to_resume{};
      bool suspend = false;
      {
        std::lock_guard<ltest::mutex> lock(queue.mutex);
        if (!queue.receivers.empty()) {
          ReceivePromise recv_req = queue.receivers.front();
          queue.receivers.pop_front();
          *(recv_req.elem) = elem;
          to_resume = recv_req.receiver;
        } else {
          id = queue.next_waiter_id++;
          sender = h;
          queue.senders.push_back(*this);
          suspend = true;
        }
      }

      if (to_resume) to_resume.resume();
      return suspend;
    }

    as_atomic void unregister() {
      if (id == 0) return;
      std::lock_guard<ltest::mutex> lock(queue.mutex);
      auto it = std::find_if(
          queue.senders.begin(), queue.senders.end(),
          [this](const SendPromise& waiter) { return waiter.id == id; });
      if (it != queue.senders.end()) {
        queue.senders.erase(it);
      }
      id = 0;
    }

    as_atomic void await_resume() {}

    std::uint64_t id{0};
    std::coroutine_handle<> sender{};
    int elem;
    CoroutineQueue& queue;
  };
};

auto generateInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

target_method_dual(ltest::generators::genEmpty, int, CoroutineQueue, receive);
target_method_dual(generateInt, void, CoroutineQueue, send, int);

using spec_t =
    ltest::SpecDual<CoroutineQueue, spec::CoroutineQueue,
                    spec::CoroutineQueueHash, spec::CoroutineQueueEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
