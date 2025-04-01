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
  const int N = 100;
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
  non_atomic Node* allocateNode(int value) {
    int currentIndex = index.fetch_add(1);  // Atomically increment the index
    assert(currentIndex < nodes.size() && "Node pool exhausted");

    Node* node = &nodes[currentIndex];
    node->value = value;
    node->next.store(nullptr);
    return node;
  }

 public:
  MSQueue() : nodes(N), index(0) {
    head.store(&dummyNode);
    tail.store(&dummyNode);
  }

  non_atomic void Push(int value) {
    Node* newNode = allocateNode(value);
    Node* currentTail = nullptr;
    Node* currentNext = nullptr;

    while (true) {
      currentTail = tail.load();
      currentNext = currentTail->next.load();

      // Check if tail is still consistent
      if (currentTail == tail.load()) {
        if (currentNext == nullptr) {
          // Try to link the new node at the end
          if (currentTail->next.compare_exchange_strong(currentNext, newNode)) {
            // Successfully added the new node
            break;
          }
        } else {
          // Tail was not pointing to the last node, try to advance it
          tail.compare_exchange_strong(currentTail, currentNext);
        }
      }
    }

    // Try to move tail to the new node
    tail.compare_exchange_strong(currentTail, newNode);
  }

  non_atomic int Pop() {
    Node* currentHead = nullptr;
    Node* currentTail = nullptr;
    Node* currentNext = nullptr;
    int value = 0;

    // MISTAKE
    int time = 0;
    while (time++ < 3 /* true */) {
      currentHead = head.load();
      currentTail = tail.load();
      currentNext = currentHead->next.load();

      // Check if head is still consistent
      if (currentHead == head.load()) {
        if (currentHead == currentTail) {
          // Queue might be empty or tail is lagging
          if (currentNext == nullptr) {
            return 0;  // Queue is empty
          }
          // Tail is lagging, try to advance it
          tail.compare_exchange_strong(currentTail, currentNext);
        } else {
          // Read the value before CAS to avoid use-after-free
          value = currentNext->value;

          // Try to move head to the next node
          if (head.compare_exchange_strong(currentHead, currentNext)) {
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
auto generateInt(size_t unused) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

// Specify target structure and it's sequential specification.
using spec_t =
    ltest::Spec<MSQueue, spec::Queue<>, spec::QueueHash<>, spec::QueueEquals<>>;

LTEST_ENTRYPOINT(spec_t);
  
target_method(generateInt, void, MSQueue, Push, int);

target_method(ltest::generators::genEmpty, int, MSQueue, Pop);
