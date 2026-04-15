#pragma once

#include <iostream>

#include "../logger.h"
#include "common.h"
#include "graph.h"

namespace ltest::wmm {

extern bool wmm_enabled;

class ExecutionGraph {
 public:
  ExecutionGraph(const ExecutionGraph&) = delete;
  ExecutionGraph& operator=(const ExecutionGraph&) = delete;
  ExecutionGraph(ExecutionGraph&&) = delete;
  ExecutionGraph& operator=(ExecutionGraph&&) = delete;

  static ExecutionGraph& getInstance() {
    static ExecutionGraph instance;  // Thread-safe in C++11 and later
    return instance;
  }

  void OnExecutionComplete() { graph.OnExecutionComplete(); }

  // Empties graph events and sets new number of threads.
  void Reset(int nThreads) {
    log() << "Reset Graph: threads=" << nThreads << "\n";
    this->nThreads = nThreads;
    this->nextLocationId = 0;

    graph.Reset(nThreads);
    graph.Print(log());
  }

  // When new location is constructed, it registers itself in the wmm-graph
  // in order to generate corresponding initialization event.
  template <class T>
  int RegisterLocation(T value) {
    int currentLocationId = nextLocationId++;
    log() << "Register location: loc-" << currentLocationId
          << ", init value=" << value << "\n";
    graph.AddWriteEvent(currentLocationId, WmmUtils::INIT_THREAD_ID,
                        MemoryOrder::SeqCst, value);

    graph.Print(log());
    return currentLocationId;
  }

  template <class T>
  std::optional<T> Load(int location, int threadId, MemoryOrder order) {
    // TODO: if we now only do real atomics, then they should be stored in
    // graph, I guess?
    log() << "Load: loc-" << location << ", thread=" << threadId
          << ", order=" << WmmUtils::OrderToString(order) << "\n";
    auto readValue = graph.AddReadEvent<T>(location, threadId, order);
    if (readValue.has_value()) {
      log() << "Read value: " << readValue.value() << "\n";
      graph.Print(log());
    }
    return readValue;
  }

  template <class T>
  void Store(int location, int threadId, MemoryOrder order, T value) {
    log() << "Store: loc-" << location << ", thread=" << threadId
          << ", order=" << WmmUtils::OrderToString(order) << ", value=" << value
          << "\n";
    graph.AddWriteEvent(location, threadId, order, value);

    if (!IsExecutionInfeasible()) {
      // on infeasible execution graph will be printed internally
      graph.Print(log());
    }
  }

  template <class T>
  std::optional<std::pair<bool, T>> ReadModifyWrite(int location, int threadId,
                                                    T* expected, T desired,
                                                    MemoryOrder success,
                                                    MemoryOrder failure) {
    log() << "RMW CAS: loc-" << location << ", thread=" << threadId
          << ", expected=" << *expected << ", desired=" << desired
          << ", success=" << WmmUtils::OrderToString(success)
          << ", failure=" << WmmUtils::OrderToString(failure) << "\n";
    auto rmwResult = graph.AddRMWEvent<T>(location, threadId, expected, desired,
                                          success, failure);
    if (rmwResult.has_value()) {
      log() << "RMW result: " << (rmwResult.value().first ? "MODIFY" : "READ")
            << ", value=" << rmwResult.value().second << "\n";
      graph.Print(log());
    }
    return rmwResult;
  }

  /// Exchange / fetch_* : returns the value read (old) before the write.
  template <class T>
  std::optional<T> UnconditionalReadModifyWrite(int location, int threadId,
                                                AtomicRmwOp op, T operand,
                                                MemoryOrder order) {
    log() << "RMW " << WmmUtils::AtomicRmwOpToString(op) << ": loc-" << location
          << ", thread=" << threadId << ", operand=" << operand
          << ", order=" << WmmUtils::OrderToString(order) << "\n";
    auto oldValue = graph.AddUnconditionalRMWEvent<T>(location, threadId, op,
                                                      operand, order);
    if (oldValue.has_value()) {
      log() << "RMW old value read: " << oldValue.value() << "\n";
      graph.Print(log());
    }
    return oldValue;
  }

 private:
  ExecutionGraph() = default;
  ~ExecutionGraph() = default;

  int nThreads = 0;
  int nextLocationId = 0;
  Graph graph;
  // TODO: here can add real atomic's name via clangpass
};

}  // namespace ltest::wmm