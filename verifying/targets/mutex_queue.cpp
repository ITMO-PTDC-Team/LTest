/**
 * ./build/verifying/targets/mutex_queue --tasks 4 --switches 1 --rounds 100000
 * --strategy tla
 */
#include <atomic>
#include <cstring>

#include "../lib/mutex.h"
#include "../specs/queue.h"

constexpr int n = 100;

struct Queue {
  Queue() { a.fill(0); }
  NON_ATOMIC void Push(std::shared_ptr<Token> token, int v) {
    mutex.Lock(token);
    a[head++] = v;
    ++cnt;
    assert(cnt == 1);
    --cnt;
    mutex.Unlock();
  }

  NON_ATOMIC int Pop(std::shared_ptr<Token> token) {
    mutex.Lock(token);
    int e = 0;
    if (head - tail > 0) {
      e = a[tail++];
    }
    ++cnt;
    assert(cnt == 1);
    --cnt;
    mutex.Unlock();
    return e;
  }

  void Reset() {
    mutex = Mutex{};
    tail = head = 0;
    cnt = 0;
    a.fill(0);
  }

  int cnt{};
  Mutex mutex{};
  int tail{}, head{};
  std::array<int, n> a;
};

namespace ltest {}  // namespace ltest

auto GenerateInt() { return ltest::generators::MakeSingleArg(rand() % 10 + 1); }

auto GenerateArgs(size_t thread_num) {
  auto token = ltest::generators::GenToken(thread_num);
  auto gen_int = GenerateInt();
  return std::tuple_cat(token, gen_int);
}

using QueueCls = spec::Queue<std::tuple<std::shared_ptr<Token>, int>, 1>;

using SpecT = ltest::Spec<Queue, QueueCls, spec::QueueHash<QueueCls>,
                          spec::QueueEquals<QueueCls>>;

LTEST_ENTRYPOINT(SpecT);

TARGET_METHOD(GenerateArgs, void, Queue, Push, std::shared_ptr<Token>, int);

TARGET_METHOD(ltest::generators::GenToken, int, Queue, Pop,
              std::shared_ptr<Token>);
