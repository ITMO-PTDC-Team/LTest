#include <coroutine>
#include <memory>
#include <optional>
#include <queue>
#include "../specs/coroutine_queue.h"

#include "../../runtime/include/verifying.h"

struct CoroutineQueue {
  struct SendPromise;
  struct ReceivePromise;

  CoroutineQueue()
      : receivers(std::queue<ReceivePromise>()),
        senders(std::queue<SendPromise>()) {}

  SendPromise Send(int elem) { return SendPromise(elem, *this); }
  ReceivePromise Receive() {
    return ReceivePromise(*this);
  }

  //  ReceivePromise Receive() { return ReceivePromise(*this); }

  size_t ReceiversCount() { return receivers.size(); }

  struct ReceivePromise {
    explicit ReceivePromise(CoroutineQueue& queue) : queue(queue) {
      elem = std::make_shared<std::optional<int>>(
          std::optional<int>(std::nullopt));
    }
    bool await_ready() { return false; }

    non_atomic void await_suspend(std::coroutine_handle<> h) {
      if (!queue.senders.empty()) {
        SendPromise send_req = queue.senders.back();
        queue.senders.pop();
        *elem = send_req.elem;
        send_req.sender();
        h();
      } else {
        receiver = h;
        queue.receivers.push(*this);
      }
    }

    std::optional<int> await_resume() { return *elem; }

    std::coroutine_handle<> receiver;
    std::shared_ptr<std::optional<int>> elem;
    CoroutineQueue& queue;
  };

  struct SendPromise {
    SendPromise(int elem, CoroutineQueue& queue) : elem(elem), queue(queue) {}

    bool await_ready() { return false; }

    void await_suspend(std::coroutine_handle<> h) {
      if (!queue.receivers.empty()) {
        ReceivePromise receiver = queue.receivers.back();
        queue.receivers.pop();
        *(receiver.elem) = elem;
        receiver.receiver();
        h();
      } else {
        sender = h;
        queue.senders.push(*this);
      }
    }

    int await_resume() { return 0; }

    std::coroutine_handle<> sender;
    int elem;
    CoroutineQueue& queue;
  };

 private:
  std::queue<ReceivePromise> receivers;
  std::queue<SendPromise> senders;
};


target_method_dual(ltest::generators::genEmpty, CoroutineQueue::ReceivePromise,
                   CoroutineQueue, Receive);

// Specify target structure and it's sequential specification.
using spec_t =
    ltest::SpecDual<CoroutineQueue, spec::CoroutineQueue<>>;

LTEST_ENTRYPOINT_DUAL(spec_t);
