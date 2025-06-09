#include <cassert>
#include <cstring>
#include <deque>
#include <functional>
#include <optional>

#include "../specs/communication.h"
#include "runtime/include/value_wrapper.h"

static std::vector<size_t> used(writer_count, false);

static int write_id = start_id;
static int read_id = start_id;
struct NonLinearCommunicationTest {
  std::deque<int> buf;
  NonLinearCommunicationTest() {}
  // TODO better support return values

  Coroutine SendHeader(int r) {
    buf.push_back(r);
    co_return;
  }

  Coroutine SendBody(int r) {
    buf.push_back(r);
    co_return;
  }

  Coroutine SendImpl(int i) {
    co_await SendHeader(write_id);
    co_await SendBody(i);
    write_id++;
    co_return;
  }
  void Send(int i) {
    assert(!used[i]);
    used[i] = true;
    SendImpl(i);
  }
  void Receive() {
    for (int i = 0; i < writer_count; i++) {
      buf.pop_front();
      int r = buf.front();
      buf.pop_front();
      assert(r < writer_count);
    }
  }
  void Reset() {
    std::fill(used.begin(), used.end(), false);
    write_id = start_id;
    buf.clear();
  }
};

auto GenerateArgs(size_t thread_num) {
  for (size_t i = 0; i < writer_count; i++) {
    if (!used[i]) {
      return ltest::generators::makeSingleArg(i);
    }
  }
  assert(false && "extra call");
}
static constexpr std::string_view send_func = "Send";
static constexpr std::string_view receive_func = "Receive";
static constexpr size_t output_thread = writer_count;
class NoReadBeforeWrite {
 public:
  bool Verify(CreatedTaskMetaData task) {
    // output from the pipe is the last thread
    if (task.name == send_func && task.thread_id == output_thread) {
      return false;
    }
    if (task.name == receive_func && task.thread_id != output_thread) {
      return false;
    }
    // // no receive before send
    if (task.name == receive_func && task.is_new &&
        write_id - start_id < writer_count) {
      return false;
    }
    return true;
  }
  void OnFinished(TaskWithMetaData task) {}
  void Reset() {}
  void UpdateState(std::string_view coro_name, int thread_id, bool) {}
};

target_method(GenerateArgs, void, NonLinearCommunicationTest, Send, int);
target_method(ltest::generators::genEmpty, void, NonLinearCommunicationTest,
              Receive);

using SpecT = ltest::Spec<NonLinearCommunicationTest, spec::CommunicationRef,
                          spec::UniqueArgsHash, spec::UniqueArgsEquals,
                          spec::UniqueArgsOptionsOverride>;

LTEST_ENTRYPOINT_CONSTRAINT(SpecT, NoReadBeforeWrite);
