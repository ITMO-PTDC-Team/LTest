#pragma once

#include "common.h"
#include "hbclock.h"

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
  HBClock clock;
  std::vector<EdgeId> edges;  // outgoing edges (e.g. `edge.to == this`)

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

enum RMWState { READ, MODIFY, UNSET };

template <class T>
struct RMWEvent : Event {
  RMWEvent(EventId id, int nThreads, int location, int threadId, T* expected,
           T desired, MemoryOrder successOrder, MemoryOrder failureOrder)
      : Event(id, EventType::RMW, nThreads, location, threadId, successOrder),
        // TODO: expcted might be uninitialized, so we should not dereference it
        // blindly, fix it
        initialExpectedValue(*expected),
        cachedExpectedValue(*expected),
        expected(expected),
        desired(desired),
        failureOrder(failureOrder),
        readFrom(nullptr) {}

  virtual std::string AsString() const override {
    std::stringstream ss;

    ss << id << ":" << WmmUtils::EventTypeToString(type)
       << "(written_expected=" << cachedExpectedValue
       << ", init_expected=" << initialExpectedValue << ", desired=" << desired
       << ", " << StateAsString(state) << ")"
       << ":T" << threadId << ":L" << location << ":"
       << "succ=" << WmmUtils::OrderToString(order)
       << ", fail=" << WmmUtils::OrderToString(failureOrder) << ":"
       << clock.AsString();

    return ss.str();
  }

  /*
    All methods below will use a correct memory order of this RMW event,
    meaning that for 'state == READ' it will use 'failureOrder', and for
    'state == MODIFY' it will use 'order' (successOrder).
  */
  virtual bool IsSeqCst() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::SeqCst;
  }

  virtual bool IsAcqRel() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::AcqRel;
  }

  virtual bool IsAcquire() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::Acquire;
  }

  virtual bool IsRelease() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::Release;
  }

  virtual bool IsRelaxed() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order == MemoryOrder::Relaxed;
  }

  virtual bool IsAtLeastAcquire() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order >= MemoryOrder::Acquire && order != MemoryOrder::Release;
  }

  virtual bool IsAtLeastRelease() const override {
    assert(state != RMWState::UNSET &&
           "RMW event's memory order is queried for its resolving");
    auto order = (state == RMWState::MODIFY) ? this->order : failureOrder;
    return order >= MemoryOrder::Release;
  }

  virtual bool IsReadRWM() const override { return state == RMWState::READ; }

  virtual bool IsModifyRMW() const override {
    return state == RMWState::MODIFY;
  }

  virtual void SetReadFromEvent(Event* event) override {
    assert((event == nullptr || event->IsWriteOrRMW()) &&
           "Read event must read from write/rmw event");
    readFrom = event;

    if (readFrom == nullptr) {
      state = RMWState::UNSET;
      *expected = initialExpectedValue;  // reset expected value
      cachedExpectedValue = initialExpectedValue;
    } else {
      T readValue = Event::GetWrittenValue<T>(readFrom);
      assert(*expected == initialExpectedValue &&
             "Expected value must be equal to initial expected value on RMW "
             "resolving");
      if (readValue == initialExpectedValue) {
        // in case of MODIFY we do not change expected value
        state = RMWState::MODIFY;
      } else {
        // in case of READ we set expected to the actually read value
        state = RMWState::READ;
        *expected = readValue;
        cachedExpectedValue = readValue;
      }
    }
  }

  virtual Event* GetReadFromEvent() const override { return readFrom; }

  T GetReadValue() const {
    assert(readFrom != nullptr &&
           "RMW event hasn't set its 'readFrom' write-event");
    return Event::GetWrittenValue<T>(readFrom);
  }

  T GetWrittenValue() const {
    assert(state != RMWState::UNSET &&
           "RMW event must have a resolved state (not UNSET)");
    if (state == RMWState::MODIFY) {
      // in case of MODIFY we return desired value
      return desired;
    } else {
      // in case of READ we assume that RMW writes the value that it reads from
      // its own 'readFrom' event (which is saved to the *expected pointer)
      return *expected;
    }
  }

  RMWState state = UNSET;  // current state of the RMW operation
  T initialExpectedValue;
  // TODO: I assume, that boost stack manipulation causes a failure when
  // dereferencing of '*expected' from different thread, and it causes sanitizer
  // to fails. Requires inverstigation to debug it.
  T cachedExpectedValue;
  T* expected;
  T desired;
  MemoryOrder failureOrder;
  // points to write/rmw-event which we read from
  Event* readFrom;

 private:
  inline static std::string StateAsString(RMWState state) {
    switch (state) {
      case RMWState::READ:
        return "READ";
      case RMWState::MODIFY:
        return "MODIFY";
      case RMWState::UNSET:
        return "UNSET";
    }
  }
};

template <class T>
T Event::GetWrittenValue(Event* event) {
  assert(event != nullptr && "Event must not be null");
  assert(event->IsWriteOrRMW() && "Only write/rmw events can write values");

  if (event->IsWrite()) {
    auto writeEvent = static_cast<WriteEvent<T>*>(event);
    return writeEvent->GetWrittenValue();
  } else {
    auto rmwEvent = static_cast<RMWEvent<T>*>(event);
    return rmwEvent->GetWrittenValue();
  }
}

template <class T>
T Event::GetReadValue(Event* event) {
  assert(event->IsReadOrRMW() && "Event must be a read/rmw event");
  if (event->IsRead()) {
    ReadEvent<T>* readEvent = static_cast<ReadEvent<T>*>(event);
    return readEvent->GetReadValue();
  } else {
    RMWEvent<T>* rmwEvent = static_cast<RMWEvent<T>*>(event);
    return rmwEvent->GetReadValue();
  }
}

}  // namespace ltest::wmm