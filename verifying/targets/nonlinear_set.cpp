#include <atomic>
#include <memory>

#include "../specs/set.h"

struct SlotsSet {
 public:
  SlotsSet() {
    for (size_t i = 0; i < N; ++i) {
      slots[i].store(0);
    }
  }

  non_atomic int Insert(int value) {
    assert(value != 0);  // zero should never be added

    size_t hash = value % N;
    for (size_t i = 0; i < N; ++i) {
      size_t idx = (hash + i) % N;
      int current = slots[idx].load();
      if (current == value) break;
      if (current == 0) {
        if (slots[idx].compare_exchange_strong(current, value)) {
          return true;
        }

        if (slots[idx].load() == value) break;
      }
    }
    return false;
  }

  non_atomic int Erase(int value) {
    assert(value != 0);

    size_t hash = value % N;
    for (size_t i = 0; i < N; ++i) {
      size_t idx = (hash + i) % N;
      int current = slots[idx].load();
      if (current == value) {
        if (slots[idx].compare_exchange_strong(current, 0)) {
          return true;
        }
      }
      if (current == 0) {
        break;
      }
    }
    return false;
  }

 private:
  static inline const int N = 100;
  std::atomic<int> slots[N];
};

// Arguments generator.
auto generateInt(size_t unused_param) {
  // single value in arguments, because to find nonlinearizable
  // scenario we need 4 operations with the same argument
  // (which is pretty hard to find)
  /*
      *--------------------*--------------------*
      |         T0         |         T1         |
      *--------------------*--------------------*
      | [1] Insert(1)      |                    |
      |                    | [2] Insert(1)      |
      | <-- 1              |                    |
      | [3] Erase(1)       |                    |
      | <-- 1              |                    |
      |                    | <-- 1              |
      |                    | [4] Insert(1)      |
      |                    | <-- 1              |
      *--------------------*--------------------*
  */
  return ltest::generators::makeSingleArg(1);
}

// Specify target structure and it's sequential specification.
using spec_t =
    ltest::Spec<SlotsSet, spec::Set<>, spec::SetHash<>, spec::SetEquals<>>;

LTEST_ENTRYPOINT(spec_t);

target_method(generateInt, int, SlotsSet, Insert, int);

target_method(generateInt, int, SlotsSet, Erase, int);