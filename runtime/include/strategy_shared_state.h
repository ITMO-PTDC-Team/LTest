#pragma once

#include <sys/mman.h>

#include <cstddef>
#include <memory>
#include <new>
#include <random>

namespace ltest {

// Persistent strategy data shared between the parent and forked workers.
struct BaseStrategySharedState {
  std::mt19937 rng;

  virtual ~BaseStrategySharedState() = default;
  virtual std::unique_ptr<BaseStrategySharedState> Snapshot() const {
    return std::make_unique<BaseStrategySharedState>(*this);
  }
  virtual void Restore(const BaseStrategySharedState& snapshot) {
    rng = snapshot.rng;
  }
};

template <typename Derived>
struct StrategySharedState : BaseStrategySharedState {
  std::unique_ptr<BaseStrategySharedState> Snapshot() const override {
    return std::make_unique<Derived>(static_cast<const Derived&>(*this));
  }
  void Restore(const BaseStrategySharedState& snapshot) override {
    static_cast<Derived&>(*this) = static_cast<const Derived&>(snapshot);
  }
};

using SharedStrategyStatePtr =
    std::unique_ptr<BaseStrategySharedState, void (*)(BaseStrategySharedState*)>;

template <typename State>
SharedStrategyStatePtr MapStrategySharedState() {
  auto destroy = +[](BaseStrategySharedState* base) {
    auto* state = static_cast<State*>(base);
    state->~State();
    munmap(state, sizeof(State));
  };
  void* memory = mmap(nullptr, sizeof(State), PROT_READ | PROT_WRITE,
                      MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if (memory == MAP_FAILED) throw std::bad_alloc();
  try {
    return {new (memory) State{}, destroy};
  } catch (...) {
    munmap(memory, sizeof(State));
    throw;
  }
}

}  // namespace ltest
