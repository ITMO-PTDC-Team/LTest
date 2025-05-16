#include "verifying/specs/bank.h"

#include <cstddef>
#include <deque>
#include <shared_mutex>
#include <tuple>

#include "runtime/include/verifying.h"

class Bank {
 private:
  struct Cell {
    std::shared_mutex m;
    int amount;
    Cell(int amount) : amount(amount) {}
  };

  std::deque<Cell> cells_;

 public:
  Bank() {
    for (int i = 0; i < SIZE; ++i) {
      cells_.emplace_back(INIT / 2);
    }
  }

  non_atomic void Add(int i, size_t count) {
    // debug(stderr, "Add [%d] %lu\n", i, count);
    std::lock_guard lock{cells_[i].m};
    cells_[i].amount += count;
  }

  non_atomic int Read(int i) {
    // debug(stderr, "Read [%d]\n", i);
    std::shared_lock lock{cells_[i].m};
    return cells_[i].amount;
  }

  non_atomic int Transfer(int i, int j, size_t count) {
    // debug(stderr, "Transfer [%d] -> [%d] %lu\n", i, j, count);

    int first = std::min(i, j);
    int second = std::max(i, j);
    int res;
    if (first == second) {
      std::shared_lock lock_first{cells_[first].m};
      res = count <= cells_[i].amount;
    } else {
      std::lock_guard lock_first{cells_[first].m};
      {
        std::lock_guard lock_second{cells_[second].m};
        if (cells_[i].amount < count) {
          res = 0;
        } else {
          cells_[i].amount -= count;
          cells_[j].amount += count;
          res = 1;
        }
      }
    }
    return res;
  }

  non_atomic int ReadBoth(int i, int j) {
    // debug(stderr, "ReadBoth [%d], [%d] \n", i, j);
    int first = std::min(i, j);
    int second = std::max(i, j);
    int res;
    if (first == second) {
      std::shared_lock lock_first{cells_[first].m};
      res = cells_[i].amount * 2;
    } else {
      std::shared_lock lock_first{cells_[first].m};
      {
        std::shared_lock lock_second{cells_[second].m};
        res = cells_[i].amount + cells_[j].amount;
      }
    }
    return res;
  }
};

auto generateAdd(size_t) {
  return std::make_tuple<int, size_t>(rand() % SIZE, rand() % INIT + 1);
}

auto generateRead(size_t) { return std::make_tuple<int>(rand() % SIZE); }

auto generateTransfer(size_t) {
  return std::make_tuple<int, int, size_t>(rand() % SIZE, rand() % SIZE,
                                           rand() % INIT + 1);
}

auto generateReadBoth(size_t) {
  return std::make_tuple<int, int>(rand() % SIZE, rand() % SIZE);
}

using spec_t = ltest::Spec<Bank, spec::LinearBank, spec::LinearBankHash,
                           spec::LinearBankEquals>;

LTEST_ENTRYPOINT(spec_t);

target_method(generateAdd, void, Bank, Add, int, size_t);
target_method(generateRead, int, Bank, Read, int);
target_method(generateTransfer, int, Bank, Transfer, int, int, size_t);
target_method(generateReadBoth, int, Bank, ReadBoth, int, int);