//
// Created by d84370027 on 12/22/2025.
//
// paper https://www.cs.rochester.edu/research/synchronization/pseudocode/duals.html
#include <atomic>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <vector>

#include "../specs/dual_stack_split.h"

namespace ltest {
template<>
inline std::string toString<spec::Ticket>(spec::Ticket const& t) {
  return spec::to_string(t);
}
}

struct DualTreiberStack {
  struct Node;

  struct TPtr {
    Node* ptr;
    bool  is_request;
    bool  data_underneath;
  };

  struct Node {
    int data;
    std::atomic<Node*> data_node;
    TPtr next;

    Node()
        : data(0),
          data_node(nullptr),
          next(TPtr{nullptr, false, false}) {}
  };

  using Ticket = spec::Ticket;

  std::atomic<std::uint64_t> head;
  std::atomic<int>           next_ticket_;
  std::vector<int>           simple_stack_;
  std::vector<Node*>         pending_requests_;

  static std::vector<Ticket> tickets;

  DualTreiberStack()
      : head(0),
        next_ticket_(1) {
    ResetTickets();
  }

 private:
  static constexpr std::uint64_t PTR_MASK  = (std::uint64_t(1) << 48) - 1;
  static constexpr std::uint64_t BIT_REQ   = std::uint64_t(1) << 48;
  static constexpr std::uint64_t BIT_UNDER = std::uint64_t(1) << 49;
  static constexpr int           SN_SHIFT  = 50;
  static constexpr std::uint64_t SN_MASK   = (std::uint64_t(1) << 14) - 1;

  struct HeadFields {
    Node* ptr;
    bool  is_request;
    bool  data_underneath;
    int   sn;
  };

  static std::uint64_t packHead(const HeadFields& h) {
    std::uint64_t p  = reinterpret_cast<std::uint64_t>(h.ptr) & PTR_MASK;
    std::uint64_t bits = 0;
    if (h.is_request)      bits |= BIT_REQ;
    if (h.data_underneath) bits |= BIT_UNDER;
    std::uint64_t sn_field =
        (std::uint64_t(h.sn) & SN_MASK) << SN_SHIFT;
    return p | bits | sn_field;
  }

  static HeadFields unpackHead(std::uint64_t raw) {
    HeadFields h;
    h.ptr             = reinterpret_cast<Node*>(raw & PTR_MASK);
    h.is_request      = (raw & BIT_REQ)   != 0;
    h.data_underneath = (raw & BIT_UNDER) != 0;
    h.sn  = int((raw >> SN_SHIFT) & SN_MASK);
    return h;
  }

  static bool equal_ct(const HeadFields& a, const HeadFields& b) {
    return a.ptr == b.ptr &&
           a.is_request == b.is_request &&
           a.data_underneath == b.data_underneath &&
           a.sn == b.sn;
  }

  HeadFields load_head(std::uint64_t& raw_out) const {
    raw_out = head.load(std::memory_order_seq_cst);
    return unpackHead(raw_out);
  }

  bool cas_head(std::uint64_t& expected_raw, const HeadFields& desired) {
    std::uint64_t desired_raw = packHead(desired);
    return head.compare_exchange_strong(
        expected_raw, desired_raw,
        std::memory_order_seq_cst,
        std::memory_order_seq_cst);
  }

 public:

  as_atomic void Push(int v) {
    if (!pending_requests_.empty()) {
      Node* req = pending_requests_.back();
      pending_requests_.pop_back();

      Node* data = new Node;
      data->data = v;
      req->data_node.store(data, std::memory_order_seq_cst);
      return;
    }

    simple_stack_.push_back(v);
  }


  as_atomic Ticket PopRequest() {
    Node* req = new Node;
    req->data_node.store(nullptr, std::memory_order_relaxed);
    req->next = TPtr{nullptr, false, false};

    if (!simple_stack_.empty()) {
      Node* data = new Node;
      data->data = simple_stack_.back();
      simple_stack_.pop_back();
      req->data_node.store(data, std::memory_order_seq_cst);
    } else {
      pending_requests_.push_back(req);
    }

    int id = next_ticket_.fetch_add(1, std::memory_order_seq_cst);
    Ticket t{id, static_cast<void*>(req)};
    RegisterTicketForGenerator(t);
    return t;
  }


  as_atomic int TryPopFollowUp(Ticket t) {
    if (t.id == 0) {
      return 0;
    }

    Node* n = static_cast<Node*>(t.impl_ptr);
    assert(n != NULL);

    Node* dn = n->data_node.exchange(nullptr, std::memory_order_seq_cst);
    if (dn == NULL) {
      return 0;
    }

    int result = dn->data;
    return result;
  }


  static as_atomic void ResetTickets() {
    tickets.clear();
  }

  static as_atomic void RegisterTicketForGenerator(Ticket t) {
    tickets.push_back(t);
  }

  static as_atomic Ticket AcquireTicketForFollowUp() {
    if (tickets.empty()) {
      return Ticket{0, nullptr};
    }
    std::size_t idx =
        static_cast<std::size_t>(rand()) % tickets.size();
    Ticket t = tickets[idx];
    tickets.erase(tickets.begin() + idx);
    return t;
  }
};

std::vector<DualTreiberStack::Ticket> DualTreiberStack::tickets;

auto generateInt(size_t thread_num) {
  (void)thread_num;
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

auto genEmptyArgs(size_t thread_num) {
  (void)thread_num;
  return ltest::generators::genEmpty(0);
}

auto generateTicket(size_t thread_num) {
  (void)thread_num;
  DualTreiberStack::Ticket t =
      DualTreiberStack::AcquireTicketForFollowUp();
  return ltest::generators::makeSingleArg(t);
}


using spec_t = ltest::Spec<DualTreiberStack,
                           spec::DualStackSplit<>,
                           spec::DualStackSplitHash<>,
                           spec::DualStackSplitEquals<>>;

LTEST_ENTRYPOINT(spec_t);

target_method(generateInt,      void,                DualTreiberStack, Push, int);
target_method(genEmptyArgs,    DualTreiberStack::Ticket,
                                DualTreiberStack, PopRequest);
target_method(generateTicket,  int,
                                DualTreiberStack, TryPopFollowUp,
                                DualTreiberStack::Ticket);
