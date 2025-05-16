

#include <cstddef>
#include <deque>
#include <functional>
#include <map>
#include <string>

#include "runtime/include/value_wrapper.h"
static constexpr size_t INIT = 100;
static constexpr size_t SIZE = 2;

namespace spec {

struct LinearBank;

using bank_method_t = std::function<ValueWrapper(LinearBank *l, void *)>;

struct LinearBank {
  std::deque<int> cells;

  LinearBank() {
    for (int i = 0; i < SIZE; ++i) {
      cells.emplace_back(INIT / 2);
    }
  }

  void Add(int i, size_t count) { cells[i] += count; }

  int Read(int i) { return cells[i]; }

  int Transfer(int i, int j, size_t count) {
    if (cells[i] < count) {
      return 0;
    }
    cells[i] -= count;
    cells[j] += count;
    return 1;
  }

  int ReadBoth(int i, int j) { return cells[i] + cells[j]; }

  static auto GetMethods() {
    bank_method_t add_func = [](LinearBank *l, void *args) -> ValueWrapper {
      auto real_args = reinterpret_cast<std::tuple<int, size_t> *>(args);
      l->Add(std::get<0>(*real_args), std::get<1>(*real_args));
      return void_v;
    };

    bank_method_t read_func = [](LinearBank *l, void *args) -> int {
      auto real_args = reinterpret_cast<std::tuple<int> *>(args);
      return l->Read(std::get<0>(*real_args));
    };

    bank_method_t transfer_func = [](LinearBank *l, void *args) -> int {
      auto real_args = reinterpret_cast<std::tuple<int, int, size_t> *>(args);
      return l->Transfer(std::get<0>(*real_args), std::get<1>(*real_args),
                         std::get<2>(*real_args));
    };

    bank_method_t read_both_func = [](LinearBank *l, void *args) -> int {
      auto real_args = reinterpret_cast<std::tuple<int, int> *>(args);
      return l->ReadBoth(std::get<0>(*real_args), std::get<1>(*real_args));
    };

    return std::map<std::string, bank_method_t>{{"Add", add_func},
                                                {"Read", read_func},
                                                {"Transfer", transfer_func},
                                                {"ReadBoth", read_both_func}};
  }
};

struct LinearBankHash {
  size_t operator()(const LinearBank &r) const {
    size_t hash = 0;
    for (auto cell : r.cells) {
      hash += cell;
    }
    return hash;
  }
};

struct LinearBankEquals {
  bool operator()(const LinearBank &lhs, const LinearBank &rhs) const {
    return lhs.cells == rhs.cells;
  }
};
}  // namespace spec
