#include "../specs/coroutine_queue.h"

#include <coroutine>
#include <deque>
#include <memory>
#include <optional>

#include "../../runtime/include/verifying.h"

struct CoroutineQueue {
  struct SendPromise;
  struct ReceivePromise;

  std::deque<ReceivePromise> receivers;
  std::deque<SendPromise> senders;

  SendPromise send(int elem) { return SendPromise(elem, *this); }
  ReceivePromise receive() { return ReceivePromise(*this); }

  struct ReceivePromise {
    explicit ReceivePromise(CoroutineQueue& queue) : queue(queue) {
      elem = std::make_shared<std::optional<int>>(std::nullopt);
    }
    bool await_ready() { return false; }

    non_atomic bool await_suspend(std::coroutine_handle<> h) {
      if (!queue.senders.empty()) {
        SendPromise send_req = queue.senders.front();
        queue.senders.pop_front();
        *elem = send_req.elem;
        if (send_req.sender) send_req.sender.resume();
        return false;
      }
      receiver = h;
      queue.receivers.push_back(*this);
      return true;
    }

    int await_resume() {
      assert(elem->has_value());
      return **elem;
    }

    std::coroutine_handle<> receiver{};
    std::shared_ptr<std::optional<int>> elem;
    CoroutineQueue& queue;
  };

  struct SendPromise {
    SendPromise(int elem, CoroutineQueue& queue) : elem(elem), queue(queue) {}

    bool await_ready() { return false; }

    non_atomic bool await_suspend(std::coroutine_handle<> h) {
      if (!queue.receivers.empty()) {
        ReceivePromise recv_req = queue.receivers.front();
        queue.receivers.pop_front();
        *(recv_req.elem) = elem;
        if (recv_req.receiver) recv_req.receiver.resume();
        return false;
      }
      sender = h;
      queue.senders.push_back(*this);
      return true;
    }

    void await_resume() {}

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