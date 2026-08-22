#include "os_simulator.h"

#include "block_manager.h"
#include "blocking_primitives.h"
#include "coro_ctx_guard.h"
#include "pthread_key.h"

namespace ltest {

std::optional<std::variant<CreatedThreadInfo, JoinThreadInfo>> thread_info;
uint64_t current_max_thread_id = 0;

}  // namespace ltest

extern "C" {
__attribute__((weak)) extern void* __start_ltest_reset[];
__attribute__((weak)) extern void* __stop_ltest_reset[];
__attribute__((weak)) extern void* __start_ltest_init[];
__attribute__((weak)) extern void* __stop_ltest_init[];
}

namespace ltest {

void RunStaticFunctions(void** begin, void** end) {
  for (auto p = begin; p != end; ++p) {
    auto fn = reinterpret_cast<void (*)()>(*p);
    // It can be padding, which should be ignored.
    if (fn != nullptr) {
      fn();
    }
  }
}

void ResetStaticStorage() {
  RunStaticFunctions(__start_ltest_reset, __stop_ltest_reset);
}

void RunStaticConstructors() {
  CoroCtxGuard guard;
  RunStaticFunctions(__start_ltest_init, __stop_ltest_init);
}

OSSimulator::OSSimulator() {
  memory_handler = &os_memory;
  ResetStaticStorage();
  RunStaticConstructors();
}

template <class... Ts>
struct Overloads : Ts... {
  using Ts::operator()...;
};

std::vector<std::string> PrintThreadArgs(const std::shared_ptr<void>&) {
  return {""};
}
void OSSimulator::ResetOSState() {
  mutexes.clear();
  cond_variables.clear();
  shared_mutexes.clear();
  block_manager.UnblockAll();
  context::fiber_context::FreeForgottenStacks();
  ResetPthreadKeyValues();
  ResetStaticStorage();
  memory_handler->FreeAllMemory();
  current_max_thread_id = 0;
  RunStaticConstructors();
  join_pairs.clear();
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

void OSSimulator::UpdateOSState(size_t thread, Scheduler::SeqHistory& seq,
                                FullHistoryWithThreads& full) {
  if (!thread_info.has_value()) {
    return;
  }
  std::visit(Overloads{[this, &seq, &full](CreatedThreadInfo& info) {
                         StableVector<Task> vec;
                         threads.emplace_back();
                         threads.back().emplace_back(Coro<void*>::New(
                             [info](void* v) {
                               return ValueWrapper(info.function(v),
                                                   GetDefaultCompator<void*>(),
                                                   &GetThreadResultToString);
                             },
                             nullptr, std::make_shared<std::tuple<>>(),
                             &PrintThreadArgs, info.name, -1));
                         threads.back().back()->Resume(threads.size() - 1);
                         seq.emplace_back(
                             Invoke(threads.back().back(), current_max_thread_id));
                         full.emplace_back(current_max_thread_id,
                                           threads.back().back());
                       },
                       [this, thread](JoinThreadInfo& info) {
                         join_pairs[thread] = {info.thread_id, info.retval};
                       }},
             *thread_info);
  thread_info.reset();
}

}  // namespace ltest
