#include "pthread_key.h"

#include <cerrno>
#include <limits>
#include <unordered_map>

#include "coro_ctx_guard.h"
#include "lib.h"

namespace ltest {
namespace {

struct PthreadKeyState {
  void* value = nullptr;
  bool mocked = false;
};

std::unordered_map<pthread_key_t, PthreadKeyState>& PthreadKeys() {
  static std::unordered_map<pthread_key_t, PthreadKeyState> keys;
  return keys;
}

}  // namespace

void RegisterRealPthreadKey(pthread_key_t key) {
  SchedCtxGuard guard;
  PthreadKeys().try_emplace(key);
}

int CreateMockPthreadKey(pthread_key_t* key) {
  SchedCtxGuard guard;
  auto& keys = PthreadKeys();
  constexpr std::size_t kMaxMockKeys = PTHREAD_KEYS_MAX;
  constexpr auto kFirstMockKey = std::numeric_limits<pthread_key_t>::max();
  for (std::size_t offset = 0; offset < kMaxMockKeys; ++offset) {
    const auto candidate = static_cast<pthread_key_t>(kFirstMockKey - offset);
    if (keys.try_emplace(candidate, PthreadKeyState{nullptr, true}).second) {
      *key = candidate;
      return 0;
    }
  }
  return EAGAIN;
}

bool IsMockPthreadKey(pthread_key_t key) {
  SchedCtxGuard guard;
  const auto& keys = PthreadKeys();
  const auto it = keys.find(key);
  return it != keys.end() && it->second.mocked;
}

void DeletePthreadKey(pthread_key_t key) {
  SchedCtxGuard guard;
  PthreadKeys().erase(key);
}

void* GetPthreadKeyValue(pthread_key_t key) {
  SchedCtxGuard guard;
  const auto& keys = PthreadKeys();
  const auto it = keys.find(key);
  return it == keys.end() ? nullptr : it->second.value;
}

int SetPthreadKeyValue(pthread_key_t key, const void* value) {
  SchedCtxGuard guard;
  auto& keys = PthreadKeys();
  // A library can create a key before libpreload is active.  It is still a
  // valid POSIX key when it is first used inside LTest, so start tracking its
  // shared value then.  Its destructor is unavailable in this case.
  keys[key].value = const_cast<void*>(value);
  return 0;
}

bool ShouldUseMockPthreadKeys() { return ltest_initialized && ltest_coro_ctx; }

void ResetPthreadKeyValues() {
  SchedCtxGuard guard;
  auto& keys = PthreadKeys();
  for (auto it = keys.begin(); it != keys.end();) {
    if (it->second.mocked) {
      it = keys.erase(it);
    } else {
      it->second.value = nullptr;
      ++it;
    }
  }
}

}  // namespace ltest
