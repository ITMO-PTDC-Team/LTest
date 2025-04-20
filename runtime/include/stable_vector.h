#pragma once

#include <array>
#include <cstddef>
#include <new>
#include <utility>

template <typename T>
struct StableVector {
  StableVector() = default;

  StableVector(StableVector &&rhs) noexcept {
    total_size = rhs.total_size;
    for (size_t i = 0; i < entities.size(); ++i) {
      std::swap(entities[i], rhs.entities[i]);
    }
  }

  StableVector(const StableVector &rhs) = delete;

  StableVector &operator=(StableVector &&rhs) noexcept {
    total_size = rhs.total_size;
    rhs.total_size = 0;
    for (size_t i = 0; i < entities.size(); ++i) {
      std::swap(entities[i], rhs.entities[i]);
    }
    return *this;
  }

  StableVector &operator=(const StableVector &rhs) = delete;

  ~StableVector() {
    for (size_t i = 0; i < entities.size(); ++i) {
      if (entities[i]) {
        for (size_t j = 0; j < (1ULL << i) && j + (1ULL << i) < Size() + 1;
             ++j) {
          std::launder(reinterpret_cast<T *>(entities[i][j]))->~T();
        }
        ::operator delete[](entities[i],
                            static_cast<std::align_val_t>(alignof(T)));
      } else {
        break;
      }
    }
  }

  using TypeT = std::byte[sizeof(T)]; //NOLINT(modernize-avoid-c-arrays)

  template <typename... Args>
    requires std::constructible_from<T, Args...>
  T &EmplaceBack(Args &&...args) {
    const size_t index = 63 - __builtin_clzll(total_size + 1);
    if (((total_size + 1) & total_size) == 0 && !entities[index]) {
      entities[index] = ::new (static_cast<std::align_val_t>(alignof(T)))
          TypeT[1ULL << index];
    }
    const size_t internal_index = (total_size + 1) ^ (1ULL << index);
    T *t =
        ::new (entities[index][internal_index]) T(std::forward<Args>(args)...);
    ++total_size;
    return *t;
  }

  void PopBack() noexcept {
    const size_t index = 63 - __builtin_clzll(total_size);
    if (((total_size - 1) & total_size) == 0 && entities[index + 1]) {
      ::operator delete[](std::exchange(entities[index + 1], nullptr),
                          static_cast<std::align_val_t>(alignof(T)));
    }
    const size_t internal_index = total_size ^ (1ULL << index);
    std::launder(reinterpret_cast<T *>(entities[index][internal_index]))->~T();
    --total_size;
  }

  T &operator[](size_t i) noexcept {
    const size_t index = 63 - __builtin_clzll(++i);
    const size_t internal_index = i ^ (1ULL << index);
    return *std::launder(
        reinterpret_cast<T *>(entities[index][internal_index]));
  }

  const T &operator[](size_t i) const noexcept {
    const size_t index = 63 - __builtin_clzll(++i);
    const size_t internal_index = i ^ (1ULL << index);
    return *std::launder(
        reinterpret_cast<const T *>(entities[index][internal_index]));
  }

  void Resize(size_t new_size) {
    while (new_size > total_size) {
      EmplaceBack();
    }
    while (new_size < total_size) {
      PopBack();
    }
  }

  T &Front() { return this->operator[](0); }

  const T &Front() const { return this->operator[](0); }

  T &Back() { return this->operator[](total_size - 1); }

  const T &Back() const { return this->operator[](total_size - 1); }

  [[nodiscard]] size_t Size() const { return total_size; }

  [[nodiscard]] bool Empty() const { return Size() == 0; }

 private:
  std::array<TypeT *, 31> entities{};
  size_t total_size = 0;
};
