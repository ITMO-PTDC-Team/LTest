#pragma once

#include <algorithm>
#include <sstream>

#include "common.h"
#include "vector_clock.h"

namespace ltest::wmm {

struct Event {
 protected:
  Event(EventId id, EventType type, int nThreads, int location, int threadId,
        MemoryOrder order)
      : id(id),
        type(type),
        location(location),
        threadId(threadId),
        order(order),
        clock(nThreads) {}

 public:
  EventId id;
  EventType type;
  int location;
  int threadId;
  MemoryOrder order;
  VectorClock clock;
  std::vector<EdgeId> edges;  // outgoing edges (e.g. `edge.from == this`)

  virtual ~Event() = default;

  virtual std::string AsString() const {
    std::stringstream ss;

    ss << id << ":" << WmmUtils::EventTypeToString(type) << ":T" << threadId
       << ":L" << location << ":" << WmmUtils::OrderToString(order) << ":"
       << clock.AsString();

    return ss.str();
  }

  virtual void SetReadFromEvent(Event* event) {
    assert(false && "'SetReadFromEvent' can only be called on read/rmw events");
  }

  virtual Event* GetReadFromEvent() const {
    assert(false && "'GetReadFromEvent' can only be called on read/rmw events");
    return nullptr;
  }

  bool HappensBefore(Event* other) const {
    return clock.IsSubsetOf(other->clock);
  }

  bool IsDummy() const { return type == EventType::DUMMY; }

  bool IsWrite() const { return type == EventType::WRITE; }

  bool IsRead() const { return type == EventType::READ; }

  bool IsRMW() const { return type == EventType::RMW; }

  bool IsWriteOrRMW() const { return IsWrite() || IsRMW(); }

  bool IsReadOrRMW() const { return IsRead() || IsRMW(); }

  virtual bool IsSeqCst() const { return order == MemoryOrder::SeqCst; }

  virtual bool IsAcqRel() const { return order == MemoryOrder::AcqRel; }

  virtual bool IsAcquire() const { return order == MemoryOrder::Acquire; }

  virtual bool IsRelease() const { return order == MemoryOrder::Release; }

  virtual bool IsRelaxed() const { return order == MemoryOrder::Relaxed; }

  virtual bool IsAtLeastAcquire() const {
    return order >= MemoryOrder::Acquire && order != MemoryOrder::Release;
  }

  virtual bool IsAtLeastRelease() const {
    return order >= MemoryOrder::Release;
  }

  // Returns true if this event is a RMW and it is resolved to a READ state
  virtual bool IsReadRWM() const { return false; }

  // Returns true if this event is a RMW and it is resolved to a MODIFY state
  virtual bool IsModifyRMW() const { return false; }

  template <class T>
  static T GetWrittenValue(Event* event);

  template <class T>
  static T GetReadValue(Event* event);
};

struct DummyEvent : Event {
  DummyEvent(EventId id, int nThreads, int threadId)
      : Event(id, EventType::DUMMY, nThreads, -1 /* non-existing location */,
              threadId, MemoryOrder::Relaxed) {}
};

template <class T>
struct WriteEvent : Event {
  WriteEvent(EventId id, int nThreads, int location, int threadId,
             MemoryOrder order, T value)
      : Event(id, EventType::WRITE, nThreads, location, threadId, order),
        value(std::move(value)) {}  // , moBefore(-1)

  virtual std::string AsString() const {
    std::stringstream ss;

    ss << id << ":" << WmmUtils::EventTypeToString(type) << "(" << value << ")"
       << ":T" << threadId << ":L" << location << ":"
       << WmmUtils::OrderToString(order) << ":" << clock.AsString();

    return ss.str();
  }

  T GetWrittenValue() const { return value; }

  T value;
};

template <class T>
struct ReadEvent : Event {
  ReadEvent(EventId id, int nThreads, int location, int threadId,
            MemoryOrder order)
      : Event(id, EventType::READ, nThreads, location, threadId, order),
        readFrom(nullptr) {}

  virtual void SetReadFromEvent(Event* event) override {
    assert((event == nullptr || event->IsWriteOrRMW()) &&
           "Read event must read from write/rmw event");
    readFrom = event;
  }

  virtual Event* GetReadFromEvent() const override { return readFrom; }

  T GetReadValue() const {
    assert(readFrom != nullptr &&
           "Read event hasn't set its 'readFrom' write-event");
    return Event::GetWrittenValue<T>(readFrom);
  }

  // points to write-event which we read from
  Event* readFrom;
};

enum RMWState {
  READ,    // RMW is unsuccessful and will read the value without modification
  MODIFY,  // RMW is successful and will modify the value (this is the only
           // state for all non-CAS RMWs)
  UNSET    // initial state, before we know whether RMW is successful or not
};

template <class T>
T ApplyAtomicRmwOp(AtomicRmwOp op, T readValue, T operand) {
  switch (op) {
    case AtomicRmwOp::Exchange:
      return operand;
    case AtomicRmwOp::FetchAdd:
      return readValue + operand;
    case AtomicRmwOp::FetchSub:
      return readValue - operand;
    case AtomicRmwOp::FetchAnd:
      return readValue & operand;
    case AtomicRmwOp::FetchOr:
      return readValue | operand;
    case AtomicRmwOp::FetchXor:
      return readValue ^ operand;
    case AtomicRmwOp::FetchMin:
      return std::min(readValue, operand);
    case AtomicRmwOp::FetchMax:
      return std::max(readValue, operand);
    default:
      assert(false && "unknown AtomicRmwOp");
      return readValue;
  }
}

/// Common base for compare-exchange (CAS) and unconditional RMW events.
template <class T>
struct RMWEventBase : Event {
 protected:
  RMWEventBase(EventId id, int nThreads, int location, int threadId,
               MemoryOrder order)
      : Event(id, EventType::RMW, nThreads, location, threadId, order),
        readFrom(nullptr) {}

  static std::string StateAsString(RMWState s) {
    switch (s) {
      case RMWState::READ:
        return "READ";
      case RMWState::MODIFY:
        return "MODIFY";
      case RMWState::UNSET:
        return "UNSET";
    }
  }

 public:
  Event* readFrom;         // points to write/rmw-event which we read from
  RMWState state = UNSET;  // current state of the RMW operation

  virtual Event* GetReadFromEvent() const override { return readFrom; }
  virtual T GetReadValue() const = 0;
  virtual T GetWrittenValue() const = 0;
};

/// Compare-exchange: may resolve to READ (fail) or MODIFY (success).
template <class T>
struct CASRMWEvent : RMWEventBase<T> {
  CASRMWEvent(EventId id, int nThreads, int location, int threadId, T* expected,
              T desired, MemoryOrder successOrder, MemoryOrder failureOrder)
      : RMWEventBase<T>(id, nThreads, location, threadId, successOrder),
        // TODO: `expected` might be uninitialized, so we should not dereference
        // it blindly, fix it
        initialExpectedValue(*expected),
        cachedExpectedValue(*expected),
        expected(expected),
        desired(desired),
        failureOrder(failureOrder) {}

  virtual std::string AsString() const override {
    std::stringstream ss;

    ss << this->id << ":" << WmmUtils::EventTypeToString(this->type) << "(CAS"
       << ", written_expected=" << cachedExpectedValue
       << ", init_expected=" << initialExpectedValue << ", desired=" << desired
       << ", " << this->StateAsString(this->state) << ")"
       << ":T" << this->threadId << ":L" << this->location << ":"
       << "succ=" << WmmUtils::OrderToString(this->order)
       << ", fail=" << WmmUtils::OrderToString(failureOrder) << ":"
       << this->clock.AsString();

    return ss.str();
  }

  /*
    All methods below will use a correct memory order of this RMW event,
    meaning that for 'state == READ' it will use 'failureOrder', and for
    'state == MODIFY' it will use 'order' (successOrder).
  */
  virtual bool IsSeqCst() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::SeqCst;
  }

  virtual bool IsAcqRel() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::AcqRel;
  }

  virtual bool IsAcquire() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::Acquire;
  }

  virtual bool IsRelease() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::Release;
  }

  virtual bool IsRelaxed() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::Relaxed;
  }

  virtual bool IsAtLeastAcquire() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order >= MemoryOrder::Acquire && order != MemoryOrder::Release;
  }

  virtual bool IsAtLeastRelease() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (this->state == RMWState::MODIFY) ? this->order : failureOrder;
    return order >= MemoryOrder::Release;
  }

  virtual bool IsReadRWM() const override {
    return this->state == RMWState::READ;
  }

  virtual bool IsModifyRMW() const override {
    return this->state == RMWState::MODIFY;
  }

  virtual void SetReadFromEvent(Event* event) override {
    assert((event == nullptr || event->IsWriteOrRMW()) &&
           "Read event must read from write/rmw event");
    this->readFrom = event;

    if (this->readFrom == nullptr) {
      this->state = RMWState::UNSET;
      *expected = initialExpectedValue;  // reset expected value
      cachedExpectedValue = initialExpectedValue;
    } else {
      T readValue = Event::GetWrittenValue<T>(this->readFrom);
      assert(*expected == initialExpectedValue &&
             "Expected value must be equal to initial expected value on RMW "
             "resolving");
      if (readValue == initialExpectedValue) {
        // in case of MODIFY we do not change expected value
        this->state = RMWState::MODIFY;
      } else {
        // in case of READ we set expected to the actually read value
        this->state = RMWState::READ;
        *expected = readValue;
        cachedExpectedValue = readValue;
      }
    }
  }

  T GetReadValue() const override {
    assert(this->readFrom != nullptr &&
           "RMW event hasn't set its 'readFrom' write-event");
    return Event::GetWrittenValue<T>(this->readFrom);
  }

  T GetWrittenValue() const override {
    assert(this->state != RMWState::UNSET &&
           "RMW event must have a resolved state (not UNSET)");
    if (this->state == RMWState::MODIFY) {
      // in case of MODIFY we return desired value
      return desired;
    }
    // in case of READ we assume that RMW writes the value that it reads from
    // its own 'readFrom' event (which is saved to the *expected pointer)
    return *expected;
  }

  // Initial value of `expected` pointer, which is used for comparison in CAS
  // operation.
  T initialExpectedValue;
  // TODO: I assume, that boost stack manipulation causes a failure when
  // dereferencing of '*expected' from different thread (for example when
  // printing graph), and it causes sanitizer to fail. Requires inverstigation
  // to debug it.
  //
  // Due to the problem described in todo above, this variable is used for
  // reading the value which is currently stored in `expected` when the event is
  // printed (it could be the value that this RMW reads in case if it is a
  // failed CAS)
  T cachedExpectedValue;
  // Address of `expected` which will be modified on failed CAS semantics.
  T* expected;
  T desired;
  // Memory order in case if this RMW is CAS and it fails.
  MemoryOrder failureOrder;
};

/// Exchange / fetch_* : `state` is always MODIFY once rf is chosen (single
/// memory order).
template <class T>
struct UnconditionalRMWEvent : RMWEventBase<T> {
  UnconditionalRMWEvent(EventId id, int nThreads, int location, int threadId,
                        AtomicRmwOp op, T operand, MemoryOrder order)
      : RMWEventBase<T>(id, nThreads, location, threadId, order),
        op(op),
        operand(operand) {}

  virtual std::string AsString() const override {
    std::stringstream ss;
    ss << this->id << ":" << WmmUtils::EventTypeToString(this->type) << "("
       << WmmUtils::AtomicRmwOpToString(op) << ", operand=" << operand << ", "
       << this->StateAsString(this->state) << ")"
       << ":T" << this->threadId << ":L" << this->location << ":"
       << WmmUtils::OrderToString(this->order) << ":" << this->clock.AsString();
    return ss.str();
  }

  virtual bool IsReadRWM() const override { return false; }

  virtual bool IsModifyRMW() const override {
    return this->state == RMWState::MODIFY;
  }

  virtual void SetReadFromEvent(Event* event) override {
    assert((event == nullptr || event->IsWriteOrRMW()) &&
           "Read event must read from write/rmw event");
    this->readFrom = event;

    if (this->readFrom == nullptr) {
      this->state = RMWState::UNSET;
      return;
    }

    cachedReadValue = Event::GetWrittenValue<T>(this->readFrom);
    writtenValue = ApplyAtomicRmwOp(op, cachedReadValue, operand);
    this->state = RMWState::MODIFY;
  }

  T GetReadValue() const override {
    assert(this->readFrom != nullptr &&
           "RMW event hasn't set its 'readFrom' write-event");
    return cachedReadValue;
  }

  T GetWrittenValue() const override {
    assert(this->state == RMWState::MODIFY &&
           "Unconditional RMW must be resolved to MODIFY");
    return writtenValue;
  }

  AtomicRmwOp op;
  T operand;
  T cachedReadValue{};
  T writtenValue{};
};

template <class T>
T Event::GetWrittenValue(Event* event) {
  assert(event != nullptr && "Event must not be null");
  assert(event->IsWriteOrRMW() && "Only write/rmw events can write values");

  if (event->IsWrite()) {
    auto writeEvent = static_cast<WriteEvent<T>*>(event);
    return writeEvent->GetWrittenValue();
  }
  auto rmwEvent = static_cast<RMWEventBase<T>*>(event);
  return rmwEvent->GetWrittenValue();
}

template <class T>
T Event::GetReadValue(Event* event) {
  assert(event->IsReadOrRMW() && "Event must be a read/rmw event");
  if (event->IsRead()) {
    ReadEvent<T>* readEvent = static_cast<ReadEvent<T>*>(event);
    return readEvent->GetReadValue();
  }
  auto rmwEvent = static_cast<RMWEventBase<T>*>(event);
  return rmwEvent->GetReadValue();
}

}  // namespace ltest::wmm