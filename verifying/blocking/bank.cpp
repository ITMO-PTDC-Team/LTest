#include <cstddef>
#include <deque>
#include <functional>
#include <map>
#include <mutex>
#include <random>
#include <shared_mutex>
#include <tuple>
#include "runtime/include/verifying.h"
#include "verifying/blocking/blocking_primitives.h"


static constexpr size_t INIT = 100; 
static constexpr size_t SIZE = 2;

namespace spec {

    struct LinearBank;
    
    using bank_method_t = std::function<int(LinearBank *l, void *)>;
    
    struct LinearBank {
        std::deque<int> cells;

        LinearBank() {
            for (int i = 0; i < SIZE; ++i) {
                cells.emplace_back(INIT / 2);
            }
        }

        int Add(int i, size_t count) {
            cells[i] += count;
            return 0;
        }
    
        int Read(int i) {
            return cells[i];
        }
    
        int Transfer(int i, int j, size_t count) {
            if (cells[i] < count) {
                return 0;
            }
            cells[i] -= count;
            cells[j] += count;
            return 1;
        }
        
    
        int ReadBoth(int i, int j) {
            return cells[i] + cells[j];
        }

        static auto GetMethods() {
            bank_method_t add_func = [](LinearBank *l, void *args) -> int {
              auto real_args = reinterpret_cast<std::tuple<int, size_t> *>(args);
              return l->Add(std::get<0>(*real_args), std::get<1>(*real_args));
            };
        
            bank_method_t read_func = [](LinearBank *l, void *args) -> int {
              auto real_args = reinterpret_cast<std::tuple<int> *>(args);
              return l->Read(std::get<0>(*real_args));
            };

            bank_method_t transfer_func = [](LinearBank *l, void *args) -> int {
              auto real_args = reinterpret_cast<std::tuple<int, int, size_t> *>(args);
              return l->Transfer(std::get<0>(*real_args), std::get<1>(*real_args), std::get<2>(*real_args));
            };

            bank_method_t read_both_func = [](LinearBank *l, void *args) -> int {
                auto real_args = reinterpret_cast<std::tuple<int, int> *>(args);
                return l->ReadBoth(std::get<0>(*real_args), std::get<1>(*real_args));
              };
        
            return std::map<std::string, bank_method_t>{
                {"Add", add_func},
                {"Read", read_func},
                {"Transfer", transfer_func},
                {"ReadBoth", read_both_func}
            };
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
}
  

class Bank {
private:
    struct Cell {
        safe_shared_mutex m;
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

    non_atomic int Add(int i, size_t count) {
        debug(stderr, "Add [%d] %lu\n", i, count);
        std::lock_guard lock{cells_[i].m};
        cells_[i].amount += count;
        return 0;
    }

    non_atomic int Read(int i) {
        debug(stderr, "Read [%d]\n", i);
        std::shared_lock lock{cells_[i].m};
        return cells_[i].amount;
    }

    non_atomic int Transfer(int i, int j, size_t count) {
        debug(stderr, "Transfer [%d] -> [%d] %lu\n", i, j, count);

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
        debug(stderr, "ReadBoth [%d], [%d] \n", i, j);
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

    void Reset() {
        for (int i = 0; i < SIZE; ++i) {
            std::lock_guard lock{cells_[i].m};
            cells_[i].amount = INIT / 2;
        }
    }

};


auto generateAdd(size_t) {
    return std::make_tuple<int, size_t>(rand() % SIZE, rand() % INIT + 1);
}

auto generateRead(size_t) {
    return std::make_tuple<int>(rand() % SIZE);
}

auto generateTransfer(size_t) {
    return std::make_tuple<int, int, size_t>(rand() % SIZE, rand() % SIZE, rand() % INIT + 1);
}

auto generateReadBoth(size_t) {
    return std::make_tuple<int, int>(rand() % SIZE, rand() % SIZE);
}
  
target_method(generateAdd, int, Bank, Add, int, size_t);
target_method(generateRead, int, Bank, Read, int);
target_method(generateTransfer, int, Bank, Transfer, int, int, size_t);
target_method(generateReadBoth, int, Bank, ReadBoth, int, int);


using spec_t = ltest::Spec<Bank, spec::LinearBank, spec::LinearBankHash,
                           spec::LinearBankEquals>;

LTEST_ENTRYPOINT(spec_t);
