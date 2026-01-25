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
  T Load(int location, int threadId, MemoryOrder order) {
    // TODO: if we now only do real atomics, then they should be stored in
    // graph, I guess?
    log() << "Load: loc-" << location << ", thread=" << threadId
          << ", order=" << WmmUtils::OrderToString(order) << "\n";
    T readValue = graph.AddReadEvent<T>(location, threadId, order);

    graph.Print(log());
    return readValue;
  }

  template <class T>
  void Store(int location, int threadId, MemoryOrder order, T value) {
    log() << "Store: loc-" << location << ", thread=" << threadId
          << ", order=" << WmmUtils::OrderToString(order) << ", value=" << value
          << "\n";
    graph.AddWriteEvent(location, threadId, order, value);

    graph.Print(log());
  }

  // TODO: generalize to any other type of the RMW operation
  //       (which have different method signature from compare_and_set, e.g.
  //       fetch_add, etc.)
  template <class T>
  std::pair<bool, T> ReadModifyWrite(int location, int threadId, T* expected,
                                     T desired, MemoryOrder success,
                                     MemoryOrder failure) {
    log() << "RMW: loc-" << location << ", thread=" << threadId
          << ", expected=" << *expected << ", desired=" << desired
          << ", success=" << WmmUtils::OrderToString(success)
          << ", failure=" << WmmUtils::OrderToString(failure) << "\n";
    auto rmwResult = graph.AddRMWEvent<T>(location, threadId, expected, desired,
                                          success, failure);
    graph.Print(log());
    log() << "RMW result: " << (rmwResult.first ? "MODIFY" : "READ")
          << ", value=" << rmwResult.second << "\n";
    return rmwResult;
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