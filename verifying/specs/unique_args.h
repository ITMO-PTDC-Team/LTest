#include <optional>
#include <string>
#include <utility>

#include "../../runtime/include/verifying.h"

constexpr int limit = 3;
inline std::string Print(const ValueWrapper &v) {
  auto val = v.GetValue<std::optional<int>>();
  if (!val.has_value()) {
    return "{}";
  }
  return std::to_string(*val);
}

namespace spec {
struct UniqueArgsRef {
  size_t called = 0;
  UniqueArgsRef() = default;
  UniqueArgsRef &operator=(const UniqueArgsRef &oth) { return *this; }
  ValueWrapper Get(size_t i) {
    called++;
    return {called == limit ? std::exchange(called, 0) : std::optional<int>(),
            GetDefaultCompator<std::optional<int>>(), Print};
  }

  using MethodT = std::function<ValueWrapper(UniqueArgsRef *l, void *args)>;
  static auto GetMethods() {
    MethodT get = [](UniqueArgsRef *l, void *args) {
      auto real_args = static_cast<std::tuple<size_t> *>(args);
      return l->Get(std::get<0>(*real_args));
    };

    return std::map<std::string, MethodT>{
        {"Get", get},
    };
  }
};

struct UniqueArgsHash {
  size_t operator()(const UniqueArgsRef &r) const { return r.called; }
};
struct UniqueArgsEquals {
  bool operator()(const UniqueArgsRef &lhs, const UniqueArgsRef &rhs) const {
    return lhs.called == rhs.called;
  }
};
struct UniqueArgsOptionsOverride {
  static ltest::DefaultOptions GetOptions() {
    return {.threads = limit,
            .tasks = limit,
            .switches = 100000000,
            .rounds = 10000,
            .forbid_all_same = false,
            .verbose = false,
            .strategy = "tla",
            .weights = ""};
  }
};

struct UniqueArgsVerifier {
  bool Verify(const std::string &, size_t thread_id, bool is_new) {
    if (is_new && status[thread_id]) {
      return false;
    }
    status[thread_id] = true;
    return true;
  }

  void OnFinished(Task &, size_t) {
    // intentionally do nothing
  }

  void Reset() { status.fill(false); }

  std::array<bool, limit> status;
};
}  // namespace spec
