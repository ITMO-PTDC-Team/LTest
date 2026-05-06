#include <fuzztest/fuzztest.h>
#include <gtest/gtest.h>

#include <memory>

#include "lib.h"
#include "lincheck.h"
#include "lincheck_dual.h"
#include "lincheck_recursive.h"
#include "stackfulltask_mock.h"

struct Counter {
  int count = 0;
};

template <>
struct std::hash<Counter> {
  std::size_t operator()(const Counter& c) const noexcept {
    return std::hash<int>{}(c.count);
  }
};

template <>
struct std::equal_to<Counter> {
  constexpr bool operator()(const Counter& lhs, const Counter& rhs) const {
    return lhs.count == rhs.count;
  }
};

std::shared_ptr<CoroBase> CreateMockTask(std::string name, int ret_val,
                                         void* args) {
  auto mock = std::make_shared<MockTask>();
  EXPECT_CALL(*mock, GetRetVal())
      .Times(testing::AnyNumber())
      .WillRepeatedly(testing::Return(ret_val));
  EXPECT_CALL(*mock, GetName())
      .Times(testing::AnyNumber())
      .WillRepeatedly(testing::Return(std::move(name)));
  EXPECT_CALL(*mock, GetArgs())
      .Times(testing::AnyNumber())
      .WillRepeatedly(testing::Return(args));
  mock->MarkFinishedNormally();

  return static_pointer_cast<CoroBase>(mock);
}

namespace LinearizabilityCheckerTest {
using ::testing::AnyNumber;
using ::testing::Return;
using ::testing::ReturnRefOfCopy;

struct CustomCacheState {
  int value = 0;
};

struct CustomCacheHash {
  static inline int calls = 0;

  size_t operator()(const CustomCacheState& state) const {
    ++calls;
    return std::hash<int>{}(state.value);
  }
};

struct CustomCacheEqual {
  bool operator()(const CustomCacheState& lhs,
                  const CustomCacheState& rhs) const {
    return lhs.value == rhs.value;
  }
};

std::function<int(Counter*, void*)> fetch_and_add =
    [](Counter* c, [[maybe_unused]] void* args) {
      c->count += 1;
      return c->count - 1;
    };
std::function<int(Counter*, void*)> get =
    [](Counter* c, [[maybe_unused]] void* args) { return c->count; };

TEST(LinearizabilityCheckerCounterTest, SmallLinearizableHistory) {
  Counter c{};

  LinearizabilityChecker<Counter> checker(
      LinearizabilityChecker<Counter>::MethodMap{
          {"faa", fetch_and_add},
          {"get", get},
      },
      c);

  // Have to construct unique ptr here, otherwise the destructor will be called
  // after evaluation of the argument
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task first_task = CreateMockTask("faa", 3, empty_args);
  Task second_task = CreateMockTask("get", 3, empty_args);
  Task third_task = CreateMockTask("faa", 2, empty_args);
  Task fourth_task = CreateMockTask("faa", 1, empty_args);
  Task fifth_task = CreateMockTask("faa", 0, empty_args);

  std::vector<HistoryEvent> history{};
  history.emplace_back(Invoke(first_task, 0));
  history.emplace_back(Invoke(second_task, 1));
  history.emplace_back(Invoke(third_task, 2));
  history.emplace_back(Invoke(fourth_task, 3));
  history.emplace_back(Invoke(fifth_task, 4));
  history.emplace_back(Response(fifth_task, 0, 4));
  history.emplace_back(Response(fourth_task, 1, 3));
  history.emplace_back(Response(third_task, 2, 2));
  history.emplace_back(Response(second_task, 3, 1));
  history.emplace_back(Response(first_task, 3, 0));

  EXPECT_EQ(checker.Check(history), true);
}

TEST(LinearizabilityCheckerCounterTest, RecursiveCheckerUsesCustomHash) {
  CustomCacheHash::calls = 0;

  using Checker =
      LinearizabilityCheckerRecursive<CustomCacheState,
                                      CustomCacheHash,
                                      CustomCacheEqual>;
  Checker checker(
      Checker::MethodMap{
          {"get",
           [](CustomCacheState* state, [[maybe_unused]] void* args) {
             return ValueWrapper(state->value);
           }},
      },
      CustomCacheState{});

  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());
  Task task = CreateMockTask("get", 1, empty_args);

  std::vector<HistoryEvent> history;
  history.emplace_back(Invoke(task, 0));
  history.emplace_back(Response(task, ValueWrapper(1), 0));

  EXPECT_FALSE(checker.Check(history));
  EXPECT_GT(CustomCacheHash::calls, 0);
}

TEST(LinearizabilityCheckerCounterTest, SmallUnlinearizableHistory) {
  Counter c{};

  LinearizabilityChecker<Counter> checker(
      LinearizabilityChecker<Counter>::MethodMap{
          {"faa", fetch_and_add},
          {"get", get},
      },
      c);

  // Have to construct unique ptr here, otherwise the destructor will be called
  // after evaluation of the argument
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task first_task = CreateMockTask("faa", 2, empty_args);
  Task second_task = CreateMockTask("get", 3, empty_args);
  Task third_task = CreateMockTask("faa", 100, empty_args);
  Task fourth_task = CreateMockTask("faa", 1, empty_args);
  Task fifth_task = CreateMockTask("faa", 0, empty_args);

  std::vector<HistoryEvent> history{};
  history.emplace_back(Invoke(first_task, 0));
  history.emplace_back(Invoke(second_task, 1));
  history.emplace_back(Invoke(third_task, 2));
  history.emplace_back(Invoke(fourth_task, 3));
  history.emplace_back(Invoke(fifth_task, 4));
  history.emplace_back(Response(fifth_task, 0, 4));
  history.emplace_back(Response(fourth_task, 1, 3));
  history.emplace_back(Response(third_task, 100, 2));
  history.emplace_back(Response(second_task, 3, 1));
  history.emplace_back(Response(first_task, 3, 0));

  EXPECT_EQ(checker.Check(history), false);
}

TEST(LinearizabilityCheckerCounterTest, ExtendedLinearizableHistory) {
  Counter c{};

  LinearizabilityChecker<Counter> checker(
      LinearizabilityChecker<Counter>::MethodMap{
          {"faa", fetch_and_add},
          {"get", get},
      },
      c);

  // Have to construct unique ptr here, otherwise the destructor will be called
  // after evaluation of the argument
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task first_task = CreateMockTask("faa", 2, empty_args);
  Task second_task = CreateMockTask("get", 3, empty_args);
  Task third_task = CreateMockTask("faa", 100, empty_args);
  Task fourth_task = CreateMockTask("faa", 1, empty_args);
  Task fifth_task = CreateMockTask("faa", 0, empty_args);

  std::vector<HistoryEvent> history{};
  history.emplace_back(Invoke(first_task, 0));
  history.emplace_back(Invoke(second_task, 1));
  history.emplace_back(Invoke(third_task, 2));
  history.emplace_back(Invoke(fourth_task, 3));
  history.emplace_back(Invoke(fifth_task, 4));

  EXPECT_EQ(checker.Check(history), true);
}

std::vector<Task> create_mocks(const std::vector<bool>& b_history) {
  std::vector<Task> mocks;
  mocks.reserve(b_history.size());
  size_t adds = 0;
  // TODO: lifetime of the arguments is less than lifetime of the mocks, but the
  // arguments aren't used is it ub?
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  for (auto v : b_history) {
    if (v) {
      mocks.emplace_back(CreateMockTask("faa", adds, empty_args));
      adds++;
    } else {
      mocks.emplace_back(CreateMockTask("get", adds, empty_args));
    }
  }

  return mocks;
}

std::vector<HistoryEvent> create_history(const std::vector<Task>& mocks) {
  std::vector<HistoryEvent> history;
  history.reserve(2 * mocks.size());

  for (size_t i = 0; i < mocks.size(); ++i) {
    history.emplace_back(Invoke(mocks[i], i));
    history.emplace_back(Response(mocks[i], mocks[i]->GetRetVal(), i));
  }

  std::random_device rd;
  std::mt19937 g(rd());
  std::shuffle(history.begin(), history.end(), g);

  // Fix the order between invokes and responses
  std::map<Task, size_t> responses_indexes;

  for (size_t i = 0; i < history.size(); ++i) {
    auto& event = history[i];
    if (event.index() == 0) {
      if (responses_indexes.find(std::get<0>(event).GetTask()) !=
          responses_indexes.end()) {
        size_t index = responses_indexes[std::get<0>(event).GetTask()];
        std::swap(history[index], history[i]);
      }
    } else {
      responses_indexes[std::get<1>(event).GetTask()] = i;
    }
  }

  return history;
}

std::string draw_history(const std::vector<HistoryEvent>& history) {
  std::map<Task, size_t> numeration;
  size_t i = 0;
  for (auto& event : history) {
    if (event.index() == 1) {
      continue;
    }

    Invoke invoke = std::get<Invoke>(event);
    numeration[invoke.GetTask()] = i;
    ++i;
  }

  std::stringstream history_string;

  for (auto& event : history) {
    if (event.index() == 0) {
      Invoke invoke = std::get<Invoke>(event);
      history_string << "[" << numeration[invoke.GetTask()]
                     << " inv: " << invoke.GetTask()->GetName() << "]\n";
    } else {
      Response response = std::get<Response>(event);
      history_string << "[" << numeration[response.GetTask()]
                     << " res: " << response.GetTask()->GetName()
                     << " returned: "
                     << to_string(response.GetTask()->GetRetVal()) << "]\n";
    }
  }

  return history_string.str();
}

void CheckersAreTheSame(const std::vector<bool>& b_history) {
  Counter c{};

  LinearizabilityChecker<Counter> fast(
      LinearizabilityChecker<Counter>::MethodMap{
          {"faa", fetch_and_add},
          {"get", get},
      },
      c);

  LinearizabilityCheckerRecursive<Counter> slow(
      LinearizabilityCheckerRecursive<Counter>::MethodMap{
          {"faa", fetch_and_add},
          {"get", get},
      },
      c);

  auto mocks = create_mocks(b_history);
  auto history = create_history(mocks);
  EXPECT_EQ(fast.Check(history), slow.Check(history)) << draw_history(history);
}

FUZZ_TEST(LinearizabilityCheckerCounterTest, CheckersAreTheSame);

};  // namespace LinearizabilityCheckerTest

namespace LinearizabilityDualCheckerTest {

using LinearizabilityCheckerTest::CustomCacheEqual;
using LinearizabilityCheckerTest::CustomCacheHash;
using LinearizabilityCheckerTest::CustomCacheState;

struct DualQueueState {
  int available = 0;
  std::unordered_set<int> requested;
};

struct DualRejectPendingRequestState {
  std::unordered_set<int> pending;
};

class DualTestTask final : public CoroBase {
 public:
  DualTestTask(int task_id, std::string task_name, ValueWrapper ret_val,
               void* raw_args)
      : owned_name_(std::move(task_name)), args_(raw_args) {
    id = task_id;
    name = owned_name_;
    ret = std::move(ret_val);
    MarkFinishedNormally();
  }

  std::shared_ptr<CoroBase> Restart([[maybe_unused]] void* this_ptr) override {
    return nullptr;
  }

  std::vector<std::string> GetStrArgs() const override { return {}; }

  void* GetArgs() const override { return args_; }

 private:
  std::string owned_name_;
  void* args_{};
};

Task CreateDualTask(std::string name, int task_id, ValueWrapper ret_val,
                    void* args) {
  return std::make_shared<DualTestTask>(task_id, std::move(name),
                                        std::move(ret_val), args);
}

using DualChecker = LinearizabilityDualCheckerRecursive<DualQueueState>;
using DualRejectPendingRequestChecker =
    LinearizabilityDualCheckerRecursive<DualRejectPendingRequestState>;

DualChecker MakeDualChecker(int initial_available = 0) {
  DualQueueState init;
  init.available = initial_available;

  return DualChecker(
      DualChecker::MethodMap{
          {"push",
           [](DualQueueState* state,
              [[maybe_unused]] void* args) -> ValueWrapper {
             ++state->available;
             return void_v;
           }},
          {"pop",
           DualBlockingMethod<DualQueueState>{
               [](DualQueueState* state, [[maybe_unused]] void* args,
                  int op_id) { state->requested.insert(op_id); },
               [](DualQueueState* state, [[maybe_unused]] void* args,
                  int op_id) -> std::optional<ValueWrapper> {
                 if (!state->requested.contains(op_id) ||
                     state->available == 0) {
                   return std::nullopt;
                 }
                 --state->available;
                 state->requested.erase(op_id);
                 return ValueWrapper(1);
               }}}},
      init);
}

DualRejectPendingRequestChecker MakeDualRejectPendingRequestChecker() {
  DualRejectPendingRequestState init;

  return DualRejectPendingRequestChecker(
      DualRejectPendingRequestChecker::MethodMap{
          {"wait",
           DualBlockingMethod<DualRejectPendingRequestState>{
               [](DualRejectPendingRequestState* state,
                  [[maybe_unused]] void* args, int op_id) {
                 state->pending.insert(op_id);
               },
               [](DualRejectPendingRequestState* state,
                  [[maybe_unused]] void* args,
                  int op_id) -> std::optional<ValueWrapper> {
                 if (!state->pending.contains(op_id)) {
                   return std::nullopt;
                 }
                 state->pending.erase(op_id);
                 return ValueWrapper(0);
               }}}},
      init);
}

TEST(LinearizabilityDualCheckerTest,
     RequestWithoutFollowUpIsAllowedAsPartialHistory) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task pop = CreateDualTask("pop", 1, void_v, empty_args);
  std::vector<DualHistoryEvent> history;
  history.emplace_back(RequestInvoke(pop, 0));
  history.emplace_back(RequestResponse(pop, 0));

  EXPECT_TRUE(MakeDualChecker().Check(history));
}

TEST(LinearizabilityDualCheckerTest, DualCheckerUsesCustomHash) {
  CustomCacheHash::calls = 0;

  using Checker =
      LinearizabilityDualCheckerRecursive<CustomCacheState,
                                          CustomCacheHash,
                                          CustomCacheEqual>;
  Checker checker(
      Checker::MethodMap{
          {"get",
           DualNonBlockingMethod<CustomCacheState>{
               [](CustomCacheState* state, [[maybe_unused]] void* args) {
                 return ValueWrapper(state->value);
               }}},
      },
      CustomCacheState{});

  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());
  Task task = CreateMockTask("get", 1, empty_args);

  std::vector<DualHistoryEvent> history;
  history.emplace_back(Invoke(task, 0));
  history.emplace_back(Response(task, ValueWrapper(1), 0));

  EXPECT_FALSE(checker.Check(history));
  EXPECT_GT(CustomCacheHash::calls, 0);
}

TEST(LinearizabilityDualCheckerTest,
     CompletedFollowUpWithoutAvailableValueIsRejected) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task pop = CreateDualTask("pop", 1, ValueWrapper(1), empty_args);
  std::vector<DualHistoryEvent> history;
  history.emplace_back(RequestInvoke(pop, 0));
  history.emplace_back(RequestResponse(pop, 0));
  history.emplace_back(FollowUpInvoke(pop, 0));
  history.emplace_back(FollowUpResponse(pop, ValueWrapper(1), 0));

  EXPECT_FALSE(MakeDualChecker().Check(history));
}

TEST(LinearizabilityDualCheckerTest,
     OrdinaryOperationEnablesFollowUpInMixedHistory) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task push = CreateDualTask("push", 1, void_v, empty_args);
  Task pop = CreateDualTask("pop", 2, ValueWrapper(1), empty_args);

  std::vector<DualHistoryEvent> history;
  history.emplace_back(Invoke(push, 0));
  history.emplace_back(Response(push, void_v, 0));
  history.emplace_back(RequestInvoke(pop, 1));
  history.emplace_back(RequestResponse(pop, 1));
  history.emplace_back(FollowUpInvoke(pop, 1));
  history.emplace_back(FollowUpResponse(pop, ValueWrapper(1), 1));

  EXPECT_TRUE(MakeDualChecker().Check(history));
}

TEST(LinearizabilityDualCheckerTest,
     ReadyIncompleteFollowUpIsRejectedWithoutCustomValidator) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task push = CreateDualTask("push", 1, void_v, empty_args);
  Task pop = CreateDualTask("pop", 2, ValueWrapper(1), empty_args);

  std::vector<DualHistoryEvent> history;
  history.emplace_back(Invoke(push, 0));
  history.emplace_back(Response(push, void_v, 0));
  history.emplace_back(RequestInvoke(pop, 1));
  history.emplace_back(RequestResponse(pop, 1));

  EXPECT_FALSE(MakeDualChecker().Check(history));
}

TEST(LinearizabilityDualCheckerTest,
     PendingFollowUpInvokeWithoutResponseCannotConsumeReadyState) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task push = CreateDualTask("push", 1, void_v, empty_args);
  Task pop = CreateDualTask("pop", 2, ValueWrapper(1), empty_args);

  std::vector<DualHistoryEvent> history;
  history.emplace_back(Invoke(push, 0));
  history.emplace_back(Response(push, void_v, 0));
  history.emplace_back(RequestInvoke(pop, 1));
  history.emplace_back(RequestResponse(pop, 1));
  history.emplace_back(FollowUpInvoke(pop, 1));

  EXPECT_FALSE(MakeDualChecker().Check(history));
}

TEST(LinearizabilityDualCheckerTest,
     PendingRequestInvokeCanBeDroppedBeforeRequestResponse) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task wait = CreateDualTask("wait", 1, void_v, empty_args);
  std::vector<DualHistoryEvent> history;
  history.emplace_back(RequestInvoke(wait, 0));

  EXPECT_TRUE(MakeDualRejectPendingRequestChecker().Check(history));
}

TEST(LinearizabilityDualCheckerTest,
     CompletedRequestResponseIsStillValidatedAsPartialHistory) {
  auto empty_args_unique = std::make_unique<std::tuple<>>(std::tuple<>{});
  void* empty_args = reinterpret_cast<void*>(empty_args_unique.get());

  Task wait = CreateDualTask("wait", 1, void_v, empty_args);
  std::vector<DualHistoryEvent> history;
  history.emplace_back(RequestInvoke(wait, 0));
  history.emplace_back(RequestResponse(wait, 0));

  EXPECT_FALSE(MakeDualRejectPendingRequestChecker().Check(history));
}

}  // namespace LinearizabilityDualCheckerTest
