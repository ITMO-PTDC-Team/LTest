#ifndef DUAL_STACK_SPLIT_H
#define DUAL_STACK_SPLIT_H

#include <cassert>
#include <deque>
#include <functional>
#include <map>
#include <vector>
#include <cstdint>

#include "../../runtime/include/verifying.h"
#include "runtime/include/value_wrapper.h"

namespace spec {

struct DualStackSplit {
    std::deque<int> deq{};
    std::map<int, int> fulfilled_requests{};
    std::vector<int> pending_requests{};
    int next_ticket{1};

    void Push(int v) {
        if (!pending_requests.empty()) {
            int ticket = pending_requests[0];
            pending_requests.erase(pending_requests.begin());
            fulfilled_requests[ticket] = v;
        } else {
            deq.push_back(v);
        }
    }

    int PopRequest() {
        if (deq.empty()) {
            int ticket = next_ticket++;
            pending_requests.push_back(ticket);
            return -ticket;
        } else {
            int result = deq.back();
            deq.pop_back();
            return result + 1000;
        }
    }

    int PopFollowup(int ticket) {
        if (ticket >= 1000) {
            return ticket - 1000;
        } else if (ticket < 0) {
            int request_id = -ticket;

            auto it = fulfilled_requests.find(request_id);
            if (it != fulfilled_requests.end()) {
                int result = it->second;
                fulfilled_requests.erase(it);
                return result;
            } else {
                for (int pending : pending_requests) {
                    if (pending == request_id) {
                        return 0; // еще не готово
                    }
                }
                // TODO:: throw?
                // Запрос не найден - возможно уже выполнен и удален
                return 0;
            }
        }
        // TODO:: throw?
        return 0; // ticket == 0
    }

    using method_t = std::function<ValueWrapper(DualStackSplit *l, void *args)>;

    static auto GetMethods() {
        method_t push_func = [](DualStackSplit *l, void *args) -> ValueWrapper {
            auto real_args = reinterpret_cast<std::tuple<int> *>(args);
            l->Push(std::get<0>(*real_args));
            return void_v;
        };

        method_t pop_request_func = [](DualStackSplit *l, void *args) -> ValueWrapper {
            (void)args;
            int result = l->PopRequest();
            return ValueWrapper(result);
        };

        method_t pop_followup_func = [](DualStackSplit *l, void *args) -> ValueWrapper {
            auto real_args = reinterpret_cast<std::tuple<int> *>(args);
            int result = l->PopFollowup(std::get<0>(*real_args));
            return ValueWrapper(result);
        };

        return std::map<std::string, method_t>{
            {"Push", push_func},
            {"PopRequest", pop_request_func},
            {"PopFollowup", pop_followup_func},
        };
    }
};

struct DualStackSplitHash {
    size_t operator()(const DualStackSplit &r) const {
        size_t res = 0;
        for (int elem : r.deq) {
            res = res * 31 + elem;
        }
        for (const auto& pair : r.fulfilled_requests) {
            res = res * 31 + pair.first + pair.second;
        }
        for (int pending : r.pending_requests) {
            res = res * 31 + pending;
        }
        return res;
    }
};

struct DualStackSplitEquals {
    bool operator()(const DualStackSplit &lhs, const DualStackSplit &rhs) const {
        return lhs.deq == rhs.deq &&
               lhs.fulfilled_requests == rhs.fulfilled_requests &&
               lhs.pending_requests == rhs.pending_requests;
    }
};

}  // namespace spec

#endif // DUAL_STACK_SPLIT_H