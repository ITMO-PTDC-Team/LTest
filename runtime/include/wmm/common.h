#pragma once

#include <atomic>
#include <string>

namespace ltest::wmm {

using EventId = int;
using EdgeId = int;

enum class MemoryOrder { Relaxed, Acquire, Release, AcqRel, SeqCst };

enum class EventType { DUMMY, READ, WRITE, RMW };

enum class EdgeType {
  PO,  // program order / sequenced before
  SC,  // seq-cst edge
  RF,  // reads-from
  // TODO: do we need it? since we have hb-clocks already
  // HB, // happens-before
  MO,  // modification order
  // TODO: do we need it explicitly? hb-clocks can give the same basically
  // SW, // synchronized-with
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

  inline static std::string EdgeTypeToString(EdgeType type) {
    switch (type) {
      case EdgeType::PO:
        return "po";
      case EdgeType::SC:
        return "sc";
      case EdgeType::RF:
        return "rf";
      // case EdgeType::HB: return "hb";
      // case EdgeType::SW: return "sw";
      case EdgeType::MO:
        return "mo";
    }
  }

  // thread id to which all initalization events (constructors of atomics) will
  // belong to
  inline static int INIT_THREAD_ID = 0;
};

}  // namespace ltest::wmm
