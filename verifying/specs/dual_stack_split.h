//
// Created by d84370027 on 12/22/2025.
//

#pragma once

#include <cassert>
#include <cstdint>
#include <deque>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <vector>

#include "../../runtime/include/verifying.h"
#include "runtime/include/value_wrapper.h"

namespace spec {

struct Ticket {
  int   id;
  void* impl_ptr;

  Ticket(int i = 0, void* p = nullptr) : id(i), impl_ptr(p) {}
};

inline bool operator==(const Ticket& a, const Ticket& b) {
  return a.id == b.id;
}

inline std::string to_string(const Ticket& t) {
  return "Ticket{" + std::to_string(t.id) + "}";
}

template <typename PushArgTuple = std::tuple<int>, std::size_t ValueIndex = 0>
struct DualStackSplit {
  std::deque<int> deq{};

  std::vector<int> pending_requests{};

  std::map<int, std::optional<int>> ticket_state{};

  int next_ticket{1};

  void Push(int v) {
    next_ticket++;
    if (!pending_requests.empty()) {
      int id = pending_requests.back();
      pending_requests.pop_back();

      auto& slot = ticket_state[id];
      assert(!slot.has_value());
      slot = v;
    } else {
      deq.push_back(v);
    }
  }

  Ticket PopRequest() {
    int id = next_ticket++;

    if (!deq.empty()) {
      int x = deq.back();
      deq.pop_back();
      ticket_state[id] = x;
    } else {
      ticket_state[id] = std::nullopt;
      pending_requests.push_back(id);
    }

    return Ticket{id, nullptr};
  }

  int TryPopFollowUp(Ticket t) {
    if (t.id == 0) {
      return 0;
    }

    auto it = ticket_state.find(t.id);
    assert(it != ticket_state.end());

    if (!it->second.has_value()) {
      return 0;
    }

    next_ticket++;

    int res = *(it->second);
    ticket_state.erase(it);
    return res;
  }


  using method_t =
      std::function<ValueWrapper(DualStackSplit* l, void* args)>;

  static auto GetMethods() {
    method_t push_func = [](DualStackSplit* l,
                            void* args) -> ValueWrapper {
      auto real_args = reinterpret_cast<PushArgTuple*>(args);
      l->Push(std::get<ValueIndex>(*real_args));
      return void_v;
    };

    method_t pop_req_func = [](DualStackSplit* l,
                               void* /*args*/) -> Ticket {
      return l->PopRequest();
    };

    using FollowupArgs = std::tuple<Ticket>;
    method_t try_followup_func = [](DualStackSplit* l,
                                    void* args) -> int {
      auto real_args = reinterpret_cast<FollowupArgs*>(args);
      Ticket t = std::get<0>(*real_args);
      return l->TryPopFollowUp(t);
    };

    return std::map<std::string, method_t>{
        {"Push",           push_func},
        {"PopRequest",     pop_req_func},
        {"TryPopFollowUp", try_followup_func},
    };
  }
};


template <typename DSCls = DualStackSplit<>>
struct DualStackSplitHash {
  size_t operator()(const DSCls& r) const {
    size_t h = 0;
    auto mix = [](size_t& seed, size_t v) {
      seed ^= v + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
    };

    for (int x : r.deq) {
      mix(h, std::hash<int>{}(x));
    }
    for (int id : r.pending_requests) {
      mix(h, std::hash<int>{}(id));
    }
    for (auto const& kv : r.ticket_state) {
      mix(h, std::hash<int>{}(kv.first));
      if (kv.second.has_value()) {
        mix(h, std::hash<int>{}(*kv.second));
      } else {
        mix(h, 0xDEADBEEF);
      }
    }
    mix(h, std::hash<int>{}(r.next_ticket));
    return h;
  }
};

template <typename DSCls = DualStackSplit<>>
struct DualStackSplitEquals {
  bool operator()(const DSCls& lhs, const DSCls& rhs) const {
    return lhs.deq == rhs.deq &&
           lhs.pending_requests == rhs.pending_requests &&
           lhs.ticket_state == rhs.ticket_state &&
           lhs.next_ticket == rhs.next_ticket;
  }
};

}  // namespace spec