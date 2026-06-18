#pragma once

#include <atomic>
#include <string>

namespace ltest::wmm {

using EventId = int;
using EdgeId = int;

enum class MemoryOrder { Relaxed, Acquire, Release, AcqRel, SeqCst };

enum class EventType { DUMMY, READ, WRITE, RMW };

/// Operation kind for unconditional RMWs (exchange, fetch_*, etc.).
enum class AtomicRmwOp {
  Exchange,
  FetchAdd,
  FetchSub,
  FetchAnd,
  FetchOr,
  FetchXor,
  FetchMin,
  FetchMax,
};

enum class EdgeType {
  PO,  // program order / sequenced before
  SC,  // seq-cst edge
  RF,  // reads-from
  MO,  // modification order
};

struct WmmUtils {
  inline static MemoryOrder OrderFromStd(std::memory_order order) {
    switch (order) {
      case std::memory_order_relaxed:
        return MemoryOrder::Relaxed;
      case std::memory_order_acquire:
        return MemoryOrder::Acquire;
      case std::memory_order_release:
        return MemoryOrder::Release;
      case std::memory_order_acq_rel:
        return MemoryOrder::AcqRel;
      case std::memory_order_seq_cst:
        return MemoryOrder::SeqCst;
      default:
        throw std::invalid_argument("Unsupported memory order");
    }
  }

  inline static std::string OrderToString(MemoryOrder order) {
    switch (order) {
      case MemoryOrder::Relaxed:
        return "Relaxed";
      case MemoryOrder::Acquire:
        return "Acquire";
      case MemoryOrder::Release:
        return "Release";
      case MemoryOrder::AcqRel:
        return "AcqRel";
      case MemoryOrder::SeqCst:
        return "SeqCst";
    }
  }

  inline static std::string EventTypeToString(EventType type) {
    switch (type) {
      case EventType::DUMMY:
        return "D";
      case EventType::READ:
        return "R";
      case EventType::WRITE:
        return "W";
      case EventType::RMW:
        return "RMW";
    }
  }

  inline static std::string AtomicRmwOpToString(AtomicRmwOp op) {
    switch (op) {
      case AtomicRmwOp::Exchange:
        return "exchange";
      case AtomicRmwOp::FetchAdd:
        return "fetch_add";
      case AtomicRmwOp::FetchSub:
        return "fetch_sub";
      case AtomicRmwOp::FetchAnd:
        return "fetch_and";
      case AtomicRmwOp::FetchOr:
        return "fetch_or";
      case AtomicRmwOp::FetchXor:
        return "fetch_xor";
      case AtomicRmwOp::FetchMin:
        return "fetch_min";
      case AtomicRmwOp::FetchMax:
        return "fetch_max";
    }
  }

  inline static std::string EdgeTypeToString(EdgeType type) {
    switch (type) {
      case EdgeType::PO:
        return "po";
      case EdgeType::SC:
        return "sc";
      case EdgeType::RF:
        return "rf";
      case EdgeType::MO:
        return "mo";
    }
  }

  // thread id to which all initalization events (constructors of atomics) will
  // belong to
  inline static int INIT_THREAD_ID = 0;
};

}  // namespace ltest::wmm
