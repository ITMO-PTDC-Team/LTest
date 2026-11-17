#pragma once

#include <algorithm>
#include <cassert>
#include <sstream>
#include <string>
#include <vector>

namespace ltest::wmm {
struct VectorClock {
  VectorClock() = default;
  VectorClock(int nThreads) : times(nThreads, 0) {}

  std::string AsString() const {
    std::stringstream ss;

    ss << "[";
    for (size_t i = 0; i < times.size(); ++i) {
      ss << times[i];
      if (i < times.size() - 1) {
        ss << ",";
      }
    }
    ss << "]";

    return ss.str();
  }

  bool IsSubsetOf(const VectorClock& other) const {
    // Components for threads that did not exist yet are implicitly zero.
    for (size_t i = 0; i < times.size(); ++i) {
      if (times[i] > (i < other.times.size() ? other.times[i] : 0)) {
        return false;
      }
    }

    return true;
  }

  void UniteWith(const VectorClock& other) {
    if (times.size() < other.times.size()) {
      times.resize(other.times.size(), 0);
    }

    for (size_t i = 0; i < other.times.size(); ++i) {
      times[i] = std::max(times[i], other.times[i]);
    }
  }

  void Increment(int threadId) {
    assert(threadId >= 0);
    if (static_cast<size_t>(threadId) >= times.size()) {
      times.resize(static_cast<size_t>(threadId) + 1, 0);
    }
    times[threadId]++;
  }

 private:
  std::vector<int> times;
};
}  // namespace ltest::wmm
