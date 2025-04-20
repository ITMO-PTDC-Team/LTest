#include <atomic>

#include "../specs/queue.h"

struct MSQueue {
 private:
  struct Node {
    int value;
    std::atomic<Node*> next;

    Node(int val = 0) : value(val), next(nullptr) {}
  };

  // To avoid implementation of memory reclamation strategies,
  // all nodes are stored in a fixed sized vector,
  // big enough to store reasonable number of nodes.
  // The memory is cleared by the vector destructor between rounds.
  static constexpr int n = 100;
  // Fixed-size vector to store nodes
  std::vector<Node> nodes;

  // Dummy node (member of the class)
  Node dummyNode;

  // Atomic head and tail pointers
  std::atomic<Node*> head;
  std::atomic<Node*> tail;

  // Atomic index for node allocation
  std::atomic<int> index;

  // Helper function to get the next available node from the pool
  NON_ATOMIC Node* AllocateNode(int value) {
    int current_index = index.fetch_add(1);  // Atomically increment the index
    assert(current_index < nodes.size() && "Node pool exhausted");

    Node* node = &nodes[current_index];
    node->value = value;
    node->next.store(nullptr);
    return node;
  }

 public:
  MSQueue() : nodes(n), index(0) {
    head.store(&dummyNode);
    tail.store(&dummyNode);
  }

  NON_ATOMIC void Push(int value) {
    Node* new_node = AllocateNode(value);
    Node* current_tail = nullptr;
    Node* current_next = nullptr;

    while (true) {
      current_tail = tail.load();
      current_next = current_tail->next.load();

      // Check if tail is still consistent
      if (current_tail == tail.load()) {
        if (current_next == nullptr) {
          // Try to link the new node at the end
          if (current_tail->next.compare_exchange_strong(current_next, new_node)) {
            // Successfully added the new node
            break;
          }
        } else {
          // Tail was not pointing to the last node, try to advance it
          tail.compare_exchange_strong(current_tail, current_next);
        }
      }
    }

    // Try to move tail to the new node
    tail.compare_exchange_strong(current_tail, new_node);
  }

  NON_ATOMIC int Pop() {
    Node* current_head = nullptr;
    Node* current_tail = nullptr;
    Node* current_next = nullptr;
    int value = 0;

    // MISTAKE
    int time = 0;
    while (time++ < 3 /* true */) {
      current_head = head.load();
      current_tail = tail.load();
      current_next = current_head->next.load();

      // Check if head is still consistent
      if (current_head == head.load()) {
        if (current_head == current_tail) {
          // Queue might be empty or tail is lagging
          if (current_next == nullptr) {
            return 0;  // Queue is empty
          }
          // Tail is lagging, try to advance it
          tail.compare_exchange_strong(current_tail, current_next);
        } else {
          // Read the value before CAS to avoid use-after-free
          value = current_next->value;

          // Try to move head to the next node
          if (head.compare_exchange_strong(current_head, current_next)) {
            break;
          }
        }
      }
    }

    return value;
  }

  void Reset() {
    // Reset the queue to its initial state
    index.store(0);
    dummyNode.next.store(nullptr);
    head.store(&dummyNode);
    tail.store(&dummyNode);
  }
};

// Arguments generator.
auto GenerateInt(size_t unused) {
  return ltest::generators::MakeSingleArg(rand() % 10 + 1);
}

// Specify target structure and it's sequential specification.
using SpecT =
    ltest::Spec<MSQueue, spec::Queue<>, spec::QueueHash<>, spec::QueueEquals<>>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(GenerateInt, void, MSQueue, Push, int);

TARGET_METHOD(ltest::generators::GenEmpty, int, MSQueue, Pop);
