#pragma once

#include <cstddef>
#include <memory>
#include <new>
#include <random>

#include "shared_buffer.h"

namespace ltest {

// Persistent strategy data shared between the parent and forked workers.
struct BaseStrategySharedState {
  std::mt19937 rng;

  virtual ~BaseStrategySharedState() = default;

  [[nodiscard]] virtual std::unique_ptr<BaseStrategySharedState> Snapshot() const {
    return std::make_unique<BaseStrategySharedState>(*this);
  }

  virtual void Restore(const BaseStrategySharedState& snapshot) {
    rng = snapshot.rng;
  }
};

template <typename Derived>
struct StrategySharedState : BaseStrategySharedState {
  [[nodiscard]] std::unique_ptr<BaseStrategySharedState> Snapshot() const override {
    return std::make_unique<Derived>(static_cast<const Derived&>(*this));
  }

  void Restore(const BaseStrategySharedState& snapshot) override {
    static_cast<Derived&>(*this) = static_cast<const Derived&>(snapshot);
  }
};

template <typename State>
struct MappedStrategyState {
  MappedStrategyState() { new (buffer.fixed) State{}; }
  MappedStrategyState(const MappedStrategyState&) = delete;
  MappedStrategyState& operator=(const MappedStrategyState&) = delete;

  ~MappedStrategyState() { std::destroy_at(static_cast<State*>(buffer.fixed)); }

  State& operator*() const { return *static_cast<State*>(buffer.fixed); }

  Buffer buffer{sizeof(State)};
};

}  // namespace ltest
