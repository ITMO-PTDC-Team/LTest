#include <atomic>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <mutex>
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

  static std::mutex          ticket_mutex;
  static std::vector<Ticket> tickets;

  DualTreiberStack()
      : head(0),
        next_ticket_(1) {
    std::lock_guard<std::mutex> lg(ticket_mutex);
    tickets.clear();
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

  non_atomic void Push(int v) {
    Node* n = new Node;
    n->data = v;

    while (1) {
      std::uint64_t head_raw;
      HeadFields h = load_head(head_raw);

      n->next = TPtr{h.ptr, h.is_request, h.data_underneath};

      if (h.ptr == NULL ||
          (!h.is_request && !h.data_underneath)) {
        HeadFields desired{n, false, false, h.sn + 1};
        std::uint64_t expected = head_raw;
        if (cas_head(expected, desired)) return;
      }
      else if (h.is_request) {
        TPtr next = h.ptr->next;
        Node* old = h.ptr->data_node.load(std::memory_order_seq_cst);

        HeadFields filler{n, false, true, h.sn + 1};
        std::uint64_t expected = head_raw;
        if (!cas_head(expected, filler))
          continue;

        (void)h.ptr->data_node.compare_exchange_strong(
            old, n,
            std::memory_order_seq_cst,
            std::memory_order_seq_cst);

        HeadFields after{next.ptr, next.is_request, next.data_underneath,
                         h.sn + 2};
        std::uint64_t expected2 = packHead(filler);
        (void)head.compare_exchange_strong(
            expected2, packHead(after),
            std::memory_order_seq_cst,
            std::memory_order_seq_cst);
        return;
      }
      else {
        TPtr next = h.ptr->next;
        if (next.ptr == NULL) continue;

        Node* old = next.ptr->data_node.load(std::memory_order_seq_cst);

        std::uint64_t head_now_raw =
            head.load(std::memory_order_seq_cst);
        HeadFields h_now = unpackHead(head_now_raw);
        if (!equal_ct(h, h_now)) continue;

        if (old == NULL) {
          (void)next.ptr->data_node.compare_exchange_strong(
              old, h.ptr,
              std::memory_order_seq_cst,
              std::memory_order_seq_cst);
        }

        TPtr next2 = next.ptr->next;
        HeadFields new_head{next2.ptr, next2.is_request,
                            next2.data_underneath, h.sn + 1};
        std::uint64_t expected = head_raw;
        (void)cas_head(expected, new_head);
      }
    }
  }


  non_atomic Ticket PopRequest() {
    Node* n = NULL;

    while (1) {
      std::uint64_t head_raw;
      HeadFields h = load_head(head_raw);

      if (h.ptr != NULL &&
          !h.is_request &&
          !h.data_underneath) {
        TPtr next = h.ptr->next;

        HeadFields new_head{next.ptr, next.is_request,
                            next.data_underneath, h.sn + 1};
        std::uint64_t expected = head_raw;
        if (cas_head(expected, new_head)) {
          Node* data_node = h.ptr;

          Node* req = new Node;
          req->data_node.store(data_node, std::memory_order_relaxed);
          req->next = TPtr{NULL, false, false};

          int id = next_ticket_.fetch_add(1, std::memory_order_seq_cst);
          Ticket t{id, static_cast<void*>(req)};
          RegisterTicketForGenerator(t);
          return t;
        }
      }
      else if (h.ptr == NULL || h.is_request) {
        if (n == NULL) {
          n = new Node;
          n->data_node.store(NULL, std::memory_order_relaxed);
        }
        n->next = TPtr{h.ptr, true, false};

        HeadFields desired{n, true, false, h.sn + 1};
        std::uint64_t expected = head_raw;
        if (!cas_head(expected, desired))
          continue;

        int id = next_ticket_.fetch_add(1, std::memory_order_seq_cst);
        Ticket t{id, static_cast<void*>(n)};
        RegisterTicketForGenerator(t);
        return t;
      }
      else {
        TPtr next = h.ptr->next;
        if (next.ptr == NULL) continue;

        Node* old = next.ptr->data_node.load(std::memory_order_seq_cst);

        std::uint64_t head_now_raw =
            head.load(std::memory_order_seq_cst);
        HeadFields h_now = unpackHead(head_now_raw);
        if (!equal_ct(h, h_now)) continue;

        if (old == NULL) {
          (void)next.ptr->data_node.compare_exchange_strong(
              old, h.ptr,
              std::memory_order_seq_cst,
              std::memory_order_seq_cst);
        }

        TPtr next2 = next.ptr->next;
        HeadFields new_head{next2.ptr, next2.is_request,
                            next2.data_underneath, h.sn + 1};
        std::uint64_t expected = head_raw;
        (void)cas_head(expected, new_head);
      }
    }
  }


  non_atomic int TryPopFollowUp(Ticket t) {
    if (t.id == 0) {
      return 0;
    }

    Node* n = static_cast<Node*>(t.impl_ptr);
    assert(n != NULL);

    Node* dn = n->data_node.load(std::memory_order_seq_cst);
    if (dn == NULL) {
      return 0;
    }

    std::uint64_t head_raw =
        head.load(std::memory_order_seq_cst);
    HeadFields h = unpackHead(head_raw);
    if (h.ptr == n) {
      TPtr next = n->next;
      HeadFields new_head{next.ptr, next.is_request,
                          next.data_underneath, h.sn + 1};
      std::uint64_t expected = head_raw;
      (void)cas_head(expected, new_head);
    }

    int result = dn->data;
    delete dn;
    delete n;
    return result;
  }


  static void RegisterTicketForGenerator(Ticket t) {
    std::lock_guard<std::mutex> lg(ticket_mutex);
    tickets.push_back(t);
  }

  static Ticket AcquireTicketForFollowUp() {
    std::lock_guard<std::mutex> lg(ticket_mutex);
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

std::mutex          DualTreiberStack::ticket_mutex;
std::vector<DualTreiberStack::Ticket> DualTreiberStack::tickets;

auto generateInt(size_t /*thread_num*/) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

auto genEmptyArgs(size_t /*thread_num*/) {
  return ltest::generators::genEmpty(0);
}

auto generateTicket(size_t /*thread_num*/) {
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