#include <fuzztest/fuzztest.h>
#include <gtest/gtest.h>

#include <memory>

#include "lib.h"
#include "lincheck.h"
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

  return static_pointer_cast<CoroBase>(mock);
}

namespace LinearizabilityCheckerTest {
using ::testing::AnyNumber;
using ::testing::Return;
using ::testing::ReturnRefOfCopy;

std::function<value_wrapper(Counter*, void*)> fetch_and_add =
    [](Counter* c, [[maybe_unused]] void* args) {
      c->count += 1;
      return c->count - 1;
    };
std::function<value_wrapper(Counter*, void*)> get =
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
                     << " returned: " << to_string(response.GetTask()->GetRetVal())
                     << "]\n";
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