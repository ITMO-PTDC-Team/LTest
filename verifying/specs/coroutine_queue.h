#pragma once
#include <deque>
#include <map>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "../../runtime/include/lincheck_dual.h"
#include "../../runtime/include/value_wrapper.h"

namespace spec {

// Dual sequential specification for unbuffered channel-like queue.
// send/receive are dual operations with request+follow-up semantics.
struct CoroutineQueue {
  // pending requests
  std::deque<int> waiting_receives;               // op_id
  std::deque<std::pair<int, int>> waiting_sends;  // (op_id, value)

  // readiness after matching
  std::unordered_map<int, int> ready_receive_value;  // op_id -> value
  std::unordered_set<int> ready_send;                // op_id

  // ----- request handlers -----
  void RequestSend(int op_id, int v) {
    if (!waiting_receives.empty()) {
      int recv_id = waiting_receives.front();
      waiting_receives.pop_front();
      ready_receive_value[recv_id] = v;
      ready_send.insert(op_id);
    } else {
      waiting_sends.emplace_back(op_id, v);
    }
  }

  void RequestReceive(int op_id) {
    if (!waiting_sends.empty()) {
      auto [send_id, v] = waiting_sends.front();
      waiting_sends.pop_front();
      ready_receive_value[op_id] = v;
      ready_send.insert(send_id);
    } else {
      waiting_receives.push_back(op_id);
    }
  }

  // ----- follow-up handlers -----
  std::optional<ValueWrapper> FollowUpSend(int op_id) {
    if (!ready_send.contains(op_id)) return std::nullopt;
    ready_send.erase(op_id);
    return void_v;
  }

  std::optional<ValueWrapper> FollowUpReceive(int op_id) {
    auto it = ready_receive_value.find(op_id);
    if (it == ready_receive_value.end()) return std::nullopt;
    int v = it->second;
    ready_receive_value.erase(it);
    return ValueWrapper(v);
  }

  // Dual methods table
  static auto GetDualMethods() {
    using SpecT = CoroutineQueue;
    using MapT = DualMethodMap<SpecT>;

    // send(int)
    DualRequestMethod<SpecT> send_req = [](SpecT* s, void* args, int op_id) {
      auto real_args = reinterpret_cast<std::tuple<int>*>(args);
      int v = std::get<0>(*real_args);
      s->RequestSend(op_id, v);
    };
    DualFollowUpMethod<SpecT> send_fol = [](SpecT* s, void* /*args*/,
                                            int op_id) {
      return s->FollowUpSend(op_id);
    };

    // receive()
    DualRequestMethod<SpecT> recv_req =
        [](SpecT* s, void* /*args*/, int op_id) { s->RequestReceive(op_id); };
    DualFollowUpMethod<SpecT> recv_fol = [](SpecT* s, void* /*args*/,
                                            int op_id) {
      return s->FollowUpReceive(op_id);
    };

    MapT m;
    m.emplace("send", DualBlockingMethod<SpecT>{send_req, send_fol});
    m.emplace("receive", DualBlockingMethod<SpecT>{recv_req, recv_fol});
    return m;
  }
};

// Hash/equals for possible future optimizations (not used by recursive dual
// checker)
struct CoroutineQueueHash {
  size_t operator()(const CoroutineQueue& s) const {
    size_t h = 0;
    h ^= s.waiting_receives.size() * 1315423911u;
    h ^= s.waiting_sends.size() * 2654435761u;
    h ^= s.ready_receive_value.size() * 97531u;
    h ^= s.ready_send.size() * 424242u;
    return h;
  }
};

struct CoroutineQueueEquals {
  bool operator()(const CoroutineQueue& a, const CoroutineQueue& b) const {
    return a.waiting_receives == b.waiting_receives &&
           a.waiting_sends == b.waiting_sends &&
           a.ready_receive_value == b.ready_receive_value &&
           a.ready_send == b.ready_send;
  }
};

}  // namespace spec
