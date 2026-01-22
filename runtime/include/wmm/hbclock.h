#pragma once

#include <algorithm>
#include <cassert>
#include <sstream>
#include <string>
#include <vector>

namespace ltest::wmm {
struct HBClock {
  HBClock() = default;
  HBClock(int nThreads) : times(nThreads, 0) {}

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

  bool IsSubsetOf(const HBClock& other) const {
    assert(IsSameLength(other));

    for (int i = 0; i < times.size(); ++i) {
      if (times[i] > other.times[i]) {
        return false;
      }
    }

    return true;
  }

  void UniteWith(const HBClock& other) {
    assert(IsSameLength(other));

    for (int i = 0; i < times.size(); ++i) {
      times[i] = std::max(times[i], other.times[i]);
    }
  }

  void Increment(int threadId) {
    assert(threadId >= 0 && threadId < times.size());
    times[threadId]++;
  }

 private:
  bool IsSameLength(const HBClock& other) const {
    return times.size() == other.times.size();
  }

  std::vector<int> times;
};
}  // namespace ltest::wmm