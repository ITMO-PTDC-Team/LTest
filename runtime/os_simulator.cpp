#include "os_simulator.h"

#include <pthread.h>

namespace ltest {

std::optional<std::variant<CreatedThreadInfo, JoinThreadInfo>> thread_info;
uint64_t current_max_thread_id = 0;

template <class... Ts>
struct Overloads : Ts... {
  using Ts::operator()...;
};

std::vector<std::string> PrintThreadArgs(const std::shared_ptr<void>&) {
  return {""};
}
bool OSSimulator::CanThreadContinue(std::size_t thread) {
  auto it = join_pairs.find(thread);
  if (it != join_pairs.end()) {
    auto task = threads[it->second.first].back();
    if (task->IsReturned()) {
      auto retvalue = (it->second).second;
      join_pairs.erase(it);
      if (retvalue != nullptr) {
        *retvalue = task->GetRetVal().GetValue<void*>();
      }
    } else {
      return false;
    }
  }
  return true;
}

void OSSimulator::UpdateOSState(size_t thread) {
  if (!thread_info.has_value()) {
    return;
  }
  std::visit(Overloads{[this, thread](CreatedThreadInfo& info) {
                         const int id =
                             -static_cast<int>(current_max_thread_id);
                         auto& report = attempt_session->report;
                         report.operations.emplace_back(
                             id, 0, threads.size(), report.ordering.size(),
                             threads[thread].back()->GetId());
                         threads.emplace_back();
                         threads.back().emplace_back(Coro<void*>::New(
                             [info](void* v) -> ValueWrapper {
                               pthread_exit(info.function(v));
                             },
                             nullptr, std::make_shared<std::tuple<>>(),
                             &PrintThreadArgs, info.name, id));

                         report.events.emplace_back(
                             id, DualHistoryEventKind::OrdinaryInvoke);
                       },
                       [this, thread](JoinThreadInfo& info) {
                         join_pairs[thread] = {info.thread_id, info.retval};
                       }},
             *thread_info);
  thread_info.reset();
}

}  // namespace ltest
