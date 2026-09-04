#pragma once

#include <algorithm>
#include <map>
#include <sstream>
#include <string>

namespace ltest::wmm {
// A vector clock over a dynamic set of threads: threads are added lazily
// (on first `Increment`), and a thread missing from the map is treated as
// having a logical time of zero.
struct VectorClock {
  VectorClock() = default;

  std::string AsString() const {
    std::stringstream ss;

    ss << "[";
    bool first = true;
    for (const auto& [threadId, time] : times) {
      if (!first) {
        ss << ",";
      }
      ss << threadId << ":" << time;
      first = false;
    }
    ss << "]";

    return ss.str();
  }

  bool IsSubsetOf(const VectorClock& other) const {
    for (const auto& [threadId, time] : times) {
      if (time > other.GetTime(threadId)) {
        return false;
      }
    }

    return true;
  }

  void UniteWith(const VectorClock& other) {
    for (const auto& [threadId, time] : other.times) {
      auto& mine = times[threadId];
      mine = std::max(mine, time);
    }
  }

  void Increment(int threadId) { ++times[threadId]; }

 private:
  // Missing threads are assumed to have a logical time of zero.
  int GetTime(int threadId) const {
    auto it = times.find(threadId);
    return it != times.end() ? it->second : 0;
  }

  std::map<int, int> times;
};
}  // namespace ltest::wmm
