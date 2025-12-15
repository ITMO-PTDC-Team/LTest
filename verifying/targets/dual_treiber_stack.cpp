#include <atomic>
#include <cstdint>
#include <thread>
#include <chrono>
#include <cstdlib>
#include <new>

#include "../specs/dual_stack_split.h"

struct DualStack {
private:
    struct Node {
        int value;
        Node* next;
        std::atomic<Node*> data_node;
        bool is_request;
        bool data_underneath;
        std::atomic<int> ticket_id;

        Node() : value(0), next(nullptr), data_node(nullptr),
                 is_request(false), data_underneath(false), ticket_id(0) {}

        void reset() {
            value = 0;
            next = nullptr;
            data_node.store(nullptr, std::memory_order_relaxed);
            is_request = false;
            data_underneath = false;
            ticket_id.store(0, std::memory_order_relaxed);
        }
    };

    std::atomic<Node*> head;

    // Пул памяти для узлов. TODO:: разобраться с памятью?
    static constexpr int POOL_SIZE = 10000;
    union NodeStorage {
        Node node;
        NodeStorage() {}
        ~NodeStorage() {}
    };
    NodeStorage node_pool[POOL_SIZE];
    std::atomic<int> next_free_index{0};

    Node* allocate_node() {
        int idx = next_free_index.fetch_add(1, std::memory_order_relaxed);
        if (idx >= POOL_SIZE) {
            return nullptr;
        }
        Node* node = &node_pool[idx].node;
        node->reset();
        return node;
    }

    std::atomic<int> next_ticket_id{1};

    bool is_request_fulfilled(Node* request_node, int expected_ticket) {
        int current_ticket = request_node->ticket_id.load(std::memory_order_acquire);
        if (current_ticket != expected_ticket) {
            return false;
        }

        return request_node->data_node.load(std::memory_order_acquire) != nullptr;
    }

    void init_pool() {
        for (int i = 0; i < POOL_SIZE; ++i) {
            new (&node_pool[i].node) Node();
        }
    }

public:
    DualStack() : head(nullptr) {
        init_pool();
    }

    ~DualStack() {
        for (int i = 0; i < POOL_SIZE; ++i) {
            node_pool[i].node.~Node();
        }
    }

    void Push(int value) {
        Node* n = allocate_node();
        if (!n) return;

        n->value = value;

        while (true) {
            Node* current_head = head.load(std::memory_order_acquire);
            n->next = current_head;

            if (current_head == nullptr ||
                (!current_head->is_request && !current_head->data_underneath)) {
                if (head.compare_exchange_weak(current_head, n,
                                               std::memory_order_release,
                                               std::memory_order_acquire)) {
                    return;
                }
            } else if (current_head->is_request) {
                Node* request_node = current_head;
                Node* next = request_node->next;

                int request_ticket = request_node->ticket_id.load(std::memory_order_acquire);

                n->data_underneath = true;
                if (head.compare_exchange_weak(current_head, n,
                                               std::memory_order_release,
                                               std::memory_order_acquire)) {
                    Node* expected = nullptr;
                    Node* desired = n;
                    request_node->data_node.compare_exchange_weak(
                        expected, desired,
                        std::memory_order_release,
                        std::memory_order_relaxed
                    );

                    head.compare_exchange_weak(n, next,
                                               std::memory_order_release,
                                               std::memory_order_acquire);
                    return;
                }
            } else {
                Node* data_node = current_head;
                Node* next = data_node->next;
                if (!next) continue;

                int request_ticket = next->ticket_id.load(std::memory_order_acquire);

                Node* expected = nullptr;
                Node* desired = data_node;
                if (next->data_node.compare_exchange_weak(expected, desired,
                                                         std::memory_order_release,
                                                         std::memory_order_relaxed)) {
                }

                head.compare_exchange_weak(current_head, next->next,
                                           std::memory_order_release,
                                           std::memory_order_acquire);
            }
        }
    }

    int PopRequest() {
        while (true) {
            Node* current_head = head.load(std::memory_order_acquire);

            if (current_head && !current_head->is_request &&
                !current_head->data_underneath) {
                Node* next = current_head->next;
                if (head.compare_exchange_weak(current_head, next,
                                               std::memory_order_release,
                                               std::memory_order_acquire)) {
                    return current_head->value + 1000;
                }
            } else if (current_head == nullptr || current_head->is_request) {
                Node* request_node = allocate_node();
                if (!request_node) {
                    std::this_thread::sleep_for(std::chrono::microseconds(10));
                    continue;
                }
                request_node->is_request = true;

                int ticket_id = next_ticket_id.fetch_add(1, std::memory_order_relaxed);
                request_node->ticket_id.store(ticket_id, std::memory_order_release);

                request_node->next = current_head;
                if (head.compare_exchange_weak(current_head, request_node,
                                               std::memory_order_release,
                                               std::memory_order_acquire)) {
                    return -ticket_id;
                }
            } else {
                Node* data_node = current_head;
                Node* next = data_node->next;
                if (!next) continue;

                Node* expected = nullptr;
                Node* desired = data_node;
                if (next->data_node.compare_exchange_weak(expected, desired,
                                                         std::memory_order_release,
                                                         std::memory_order_relaxed)) {
                }

                head.compare_exchange_weak(current_head, next->next,
                                           std::memory_order_release,
                                           std::memory_order_acquire);
            }
        }
    }

    int PopFollowup(int ticket) {
        if (ticket >= 1000) {
            return ticket - 1000;
        } else if (ticket < 0) {
            int request_id = -ticket;

            // Ищем request node в пуле (lock-free поиск)
            // TODO:: how to do it?
            // Внимание: это линейный поиск
            for (int i = 0; i < POOL_SIZE; ++i) {
                Node* node = &node_pool[i].node;
                int node_ticket = node->ticket_id.load(std::memory_order_acquire);

                if (node_ticket == request_id && node->is_request) {
                    Node* data_node = node->data_node.load(std::memory_order_acquire);
                    if (data_node != nullptr) {
                        int result = data_node->value;

                        Node* current_head = head.load(std::memory_order_acquire);
                        if (current_head == node) {
                            Node* next = node->next;
                            head.compare_exchange_weak(node, next,
                                                       std::memory_order_release,
                                                       std::memory_order_acquire);
                        }

                        return result;
                    } else {
                        return 0;
                    }
                }
            }

            return 0;
        }
        return 0;
    }
};

// Arguments generator
auto generateInt(size_t thread_num) {
    return ltest::generators::makeSingleArg(rand() % 100 + 1);
}

auto generateTicket(size_t thread_num) {
    int ticket = rand() % 2000 - 1000;
    return ltest::generators::makeSingleArg(ticket);
}

using spec_t = ltest::Spec<DualStack, spec::DualStackSplit, spec::DualStackSplitHash,
                           spec::DualStackSplitEquals>;

LTEST_ENTRYPOINT(spec_t);

target_method(generateInt, void, DualStack, Push, int);
target_method(ltest::generators::genEmpty, int, DualStack, PopRequest);
target_method(generateTicket, int, DualStack, PopFollowup, int);