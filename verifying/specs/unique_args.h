#include <optional>
#include <string>
#include <utility>

#include "../../runtime/include/verifying.h"

constexpr int limit = 3;
inline std::string print(const ValueWrapper &v) {
  auto val = v.GetValue<std::optional<int>>();
  if (!val.has_value()) {
    return "{}";
  }
  return std::to_string(*val);
}

namespace spec {
struct UniqueArgsRef {
  size_t called = 0;
  UniqueArgsRef() {}
  UniqueArgsRef &operator=(const UniqueArgsRef &oth) { return *this; }
  ValueWrapper Get(size_t i) {
    called++;
    return {called == limit ? std::exchange(called, 0) : std::optional<int>(),
            get_default_compator<std::optional<int>>(), print};
  }

  using method_t = std::function<ValueWrapper(UniqueArgsRef *l, void *args)>;
  static auto GetMethods() {
    method_t get = [](UniqueArgsRef *l, void *args) {
      auto real_args = reinterpret_cast<std::tuple<size_t> *>(args);
      return l->Get(std::get<0>(*real_args));
    };

    return std::map<std::string, method_t>{
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
}  // namespace spec
