#include <cassert>
#include <map>

#include "include/lincheck.h"

// get_inv_res_mapping returns map (invoke_index -> corresponding
// response_index)
std::map<size_t, size_t> get_inv_res_mapping(
    const std::vector<std::variant<Invoke, Response>> &history) {
  std::map<size_t, size_t> inv_res;     // inv -> corresponding response
  std::map<const Task *, size_t> uids;  // uid -> res

  for (size_t i = 0; i < history.size(); ++i) {
    auto el = history[i];
    if (el.index() == 0) {
      const Invoke &inv = std::get<0>(el);
      uids[&inv.GetTask()] = i;
    } else {
      const Response &res = std::get<1>(el);
      assert(uids.find(&res.GetTask()) != uids.end());
      auto inv_id = uids[&res.GetTask()];
      inv_res[inv_id] = i;
    }
  }

  return inv_res;
}

Invoke::Invoke(const Task &task, int thread_id)
    : task(task), thread_id(thread_id) {}

Response::Response(const Task &task, value_wrapper result, int thread_id)
    : task(task), result(result), thread_id(thread_id) {}

const Task &Invoke::GetTask() const { return this->task; }

const Task &Response::GetTask() const { return this->task; }
