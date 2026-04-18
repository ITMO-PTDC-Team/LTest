#pragma once

#include <algorithm>
#include <iostream>
#include <map>
#include <random>
#include <ranges>
#include <unordered_set>

#include "../../runtime/include/lib.h"
#include "../logger.h"
#include "common.h"
#include "edge.h"
#include "event.h"
#include "relseq.h"

namespace ltest::wmm {

class Graph {
 public:
  Graph() {}
  ~Graph() { Clean(); }

  void OnExecutionComplete() {
    if (IsExecutionInfeasible()) {
      // already infeasible, so we don't need to try to complete it
      return;
    }
    // TODO: implement the actual completion attempt + invoke it form strategy
    if (fullReleaseSequenceSupportEnabled && HasBrokenRelSeq()) {
      log() << "Execution is infeasible on completion\n";
      Print(log());
      SetExecutionInfeasible(true);
    }
  }

  void Reset(int nThreads) {
    Clean();
    InitThreads(nThreads);
  }

  // TODO: add `ExecutionPolicy` or other way of specifying how to create edges
  // (Random, BoundedModelChecker, etc.)
  template <class T>
  std::optional<T> AddReadEvent(int location, int threadId, MemoryOrder order) {
    EventId eventId = events.size();
    auto event = new ReadEvent<T>(eventId, nThreads, location, threadId, order);

    // establish po-edge
    CreatePoEdgeToEvent(event);  // prevInThread --po--> event

    auto shuffledEvents = GetShuffledReadFromCandidates(event);
    for (auto readFromEvent : shuffledEvents) {
      // try reading from `readFromEvent`
      if (TryCreateRfEdge(readFromEvent, event)) {
        log() << "Read event " << event->AsString() << " now reads from "
              << readFromEvent->AsString() << "\n";
        break;
      }
    }

    if (event->readFrom == nullptr) {
      // we were unable to find a write/rmw to read from because we were breking
      // some existing rel-seqs, so we reached the infeasible execution
      log() << "Execution is infeasible on read event: " << event->AsString()
            << "\n";
      SetExecutionInfeasible(true);
      Print(log());
      return std::nullopt;
    }

    assert(event->readFrom != nullptr &&
           "Read event must have appropriate write event to read from");
    assert((event->readFrom->IsWrite() || event->readFrom->IsModifyRMW()) &&
           "Read event must read from write or modifying rmw event");
    return Event::GetReadValue<T>(event);
  }

  template <class T>
  void AddWriteEvent(int location, int threadId, MemoryOrder order, T value) {
    EventId eventId = events.size();
    auto event =
        new WriteEvent<T>(eventId, nThreads, location, threadId, order, value);

    // establish po-edge
    CreatePoEdgeToEvent(event);  // prevInThread --po--> event

    if (order == MemoryOrder::SeqCst) {
      // establish sc-edge between last sc-write and event
      CreateScEdgeToEvent(event);  // prevScCstWrite --sc--> event

      // Seq-Cst / MO Consistency (if locations match)
      CreateSeqCstConsistencyEdges(event);  // prevScCstWrite --mo--> event

      // update last seq_cst write
      lastSeqCstWriteEvents[event->location] = event->id;
    }

    // Read-Write Coherence
    CreateReadWriteCoherenceEdges(event);

    // Write-Write Coherence
    CreateWriteWriteCoherenceEdges(event);

    if (HasBrokenRelSeq()) {
      log() << "Execution is infeasible on write event: " << event->AsString()
            << "\n";
      Print(log());
      SetExecutionInfeasible(true);
    }
  }

  template <class T>
  std::optional<std::pair<bool, T>> AddRMWEvent(int location, int threadId,
                                                T* expected, T desired,
                                                MemoryOrder successOrder,
                                                MemoryOrder failureOrder) {
    EventId eventId = events.size();
    auto event =
        new CASRMWEvent<T>(eventId, nThreads, location, threadId, expected,
                           desired, successOrder, failureOrder);

    // establish po-edge
    CreatePoEdgeToEvent(event);  // prevInThread --po--> event

    auto shuffledEvents = GetShuffledReadFromCandidates(event);
    for (auto readFromEvent : shuffledEvents) {
      // try reading from `readFromEvent`
      if (TryCreateRfEdge(readFromEvent, event)) {
        log() << "RMW event " << event->AsString() << " now reads from "
              << readFromEvent->AsString() << "\n";
        break;
      }
    }

    if (event->readFrom == nullptr) {
      // we were unable to find a write/rmw to read from because we were breking
      // some existing rel-seqs, so we reached the infeasible execution
      log() << "Execution is infeasible on rmw event: " << event->AsString()
            << "\n";
      SetExecutionInfeasible(true);
      Print(log());
      return std::nullopt;
    }

    assert(event->readFrom != nullptr &&
           "RMW event must have appropriate write event to read from");
    assert((event->readFrom->IsWrite() || event->readFrom->IsModifyRMW()) &&
           "RMW event must read from write or modifying rmw event");
    return std::make_optional(std::pair<bool, T>{
        event->IsModifyRMW(),  // true if RMW is resolved to MODIFY state, false
                               // if rmw failed and resolve to READ state
        Event::GetReadValue<T>(event)});
  }

  template <class T>
  std::optional<T> AddUnconditionalRMWEvent(int location, int threadId,
                                            AtomicRmwOp op, T operand,
                                            MemoryOrder order) {
    EventId eventId = events.size();
    auto event = new UnconditionalRMWEvent<T>(eventId, nThreads, location,
                                              threadId, op, operand, order);

    CreatePoEdgeToEvent(event);

    auto shuffledEvents = GetShuffledReadFromCandidates(event);
    for (auto readFromEvent : shuffledEvents) {
      if (TryCreateRfEdge(readFromEvent, event)) {
        log() << "Unconditional RMW event " << event->AsString()
              << " now reads from " << readFromEvent->AsString() << "\n";
        break;
      }
    }

    if (event->readFrom == nullptr) {
      // we were unable to find a write/rmw to read from because we were breking
      // some existing rel-seqs, so we reached the infeasible execution
      log() << "Execution is infeasible on unconditional rmw event: "
            << event->AsString() << "\n";
      SetExecutionInfeasible(true);
      Print(log());
      return std::nullopt;
    }

    assert(event->readFrom != nullptr &&
           "RMW event must have appropriate write event to read from");
    assert((event->readFrom->IsWrite() || event->readFrom->IsModifyRMW()) &&
           "RMW event must read from write or modifying rmw event");
    return Event::GetReadValue<T>(event);
  }

  template <typename Out>
  void Print(Out& os) const {
    os << "Graph edges:" << "\n";
    if (edges.empty())
      os << "<empty>";
    else {
      for (const auto& edge : edges) {
        os << events[edge.from]->AsString() << " ->"
           << WmmUtils::EdgeTypeToString(edge.type) << " "
           << events[edge.to]->AsString() << "\n";
      }
    }
    os << "Release sequences:\n";
    for (const auto& rs : establishedRelSeqs) {
      os << rs.AsString() << "\n";
    }
    os << "\n";
  }

 private:
  std::vector<Event*> GetShuffledReadFromCandidates(Event* event) {
    // Shuffle events to randomize the order of read-from edges
    // and allow for more non-sc behaviours
    auto filteredEventsView =
        events | std::views::filter([event](Event* e) {
          return ((e->IsWrite() || e->IsModifyRMW()) &&
                  e->location == event->location && e != event);
        });
    std::vector<Event*> shuffledEvents(filteredEventsView.begin(),
                                       filteredEventsView.end());
    std::ranges::shuffle(shuffledEvents, gen);

    return shuffledEvents;
  }

  // Tries to create a read-from edge between `write` and `read` events (write
  // --rf--> read). Returns `true` if edge was created, `false` otherwise.
  bool TryCreateRfEdge(Event* write, Event* read) {
    assert(write->IsWriteOrRMW() && read->IsReadOrRMW() &&
           "Write and Read events must be of correct type");
    assert(write->location == read->location &&
           "Write and Read events must be of the same location");

    StartSnapshot();

    // establish rf-edge
    AddEdge(EdgeType::RF, write->id, read->id);
    // remember the event we read from
    read->SetReadFromEvent(write);

    // update the clock if synchronized-with relation has appeared
    // and save the old clock in case snapshot is discarded
    HBClock oldClock;
    bool isClockUpdated = false;

    // update the last seq-cst write event in case of successful RMW event
    // and save the old value in case snapshot is discarded
    EventId oldLastSeqCstWriteEvent = lastSeqCstWriteEvents[read->location];

    if (read->IsRead() || read->IsReadRWM()) {
      // in case of 'read' event has actually type RMW but is resolved to READ
      // state (basically the comparison of read value with `expected` one
      // failed), then we treat it as a regular READ event
      if (read->IsSeqCst()) {
        // establish sc-edge
        CreateScEdgeToEvent(read);  // prevScCstWrite --sc--> event
        // Seq-Cst Write-Read Coherence
        CreateSeqCstReadWriteCoherenceEdges(write, read);
      }

      // Write-Read Coherence
      CreateWriteReadCoherenceEdges(write, read);

      // Read-Read Coherence
      CreateReadReadCoherenceEdges(write, read);
    } else {
      // otherwise, if the RWM event is resolved to MODIFY state
      // then we treat that as RMW event and apply rules from reads and writes
      // as well
      assert(read->IsModifyRMW() &&
             "Read event must be a successful RMW event in this code branch");

      // Apply RMW-specific rules
      // RMW / MO Consistency
      CreateRmwConsistencyEdges(write, read);

      // RMW Atomicity
      CreateRmwAtomicityEdges(write, read);

      // Applying rules both from READs and WRITEs
      if (read->IsSeqCst()) {
        // establish sc-edge
        CreateScEdgeToEvent(read);  // prevScCstWrite --sc--> event
        // Seq-Cst Write-Read Coherence
        CreateSeqCstReadWriteCoherenceEdges(write, read);
        // Seq-Cst / MO Consistency (if locations match)
        CreateSeqCstConsistencyEdges(read);  // prevScCstWrite --mo--> event

        // update last seq_cst write (will be restored in case of snapshot
        // discard)
        lastSeqCstWriteEvents[read->location] = read->id;
      }

      // Write-Read Coherence
      CreateWriteReadCoherenceEdges(write, read);

      // Read-Read Coherence
      CreateReadReadCoherenceEdges(write, read);

      // Read-Write Coherence
      CreateReadWriteCoherenceEdges(read);

      // Write-Write Coherence
      CreateWriteWriteCoherenceEdges(read);
    }

    // Applying rel-acq semantics via establishing synchronized-with relation,
    // we consider here release-sequences as well
    // TODO: check how release-sequences affect sc-edges
    std::optional<RelSeq> seq = GetReleaseSequence(read, write);
    if (seq.has_value()) {
      isClockUpdated = true;
      // save the old clock
      oldClock = read->clock;
      // instantitate a SW (synchronized-with) relation (between current read
      // and all release heads from the release-sequence)
      for (auto* head : seq->releaseHeads) {
        read->clock.UniteWith(head->clock);
      }
      establishedRelSeqs.push_back(seq.value());
    }

    bool isConsistent = IsConsistent();
    bool hasBrokenRelSeq = HasBrokenRelSeq();
    bool isValid = isConsistent && !hasBrokenRelSeq;

    if (isValid) {
      log() << "Valid graph: consistency=" << isConsistent
            << ", hasBrokenRelSeq=" << hasBrokenRelSeq << "\n";
      Print(log());
      // preserve added edges and other modifications
      ApplySnapshot();
    } else {
      log() << "Not valid graph: consistency=" << isConsistent
            << ", hasBrokenRelSeq=" << hasBrokenRelSeq << "\n";
      Print(log());
      // removes all added edges
      log() << "Discarding snapshot" << "\n";
      DiscardSnapshot();
      // remove rf-edge
      read->SetReadFromEvent(nullptr);
      // restore old clock if necessary
      if (isClockUpdated) {
        read->clock = oldClock;
        // removed added release-sequence
        establishedRelSeqs.pop_back();
      }
      // restore last seq-cst write event
      lastSeqCstWriteEvents[read->location] = oldLastSeqCstWriteEvent;
      Print(log());
    }

    return isValid;
  }

  // We calculate a release sequence for a `read` event that reads-from a
  // `write` event. In case if `fullReleaseSequenceSupportEnabled` is set to
  // false, we treat direct acquire-read from a release-write/rmw as a release
  // sequence only.
  std::optional<RelSeq> GetReleaseSequence(Event* read, Event* write) const {
    assert(read->IsReadOrRMW() && "Read event must be of correct type");
    assert(write->IsWriteOrRMW() && "Write event must be of correct type");
    if (!fullReleaseSequenceSupportEnabled) {
      // Old behavior when no release-sequences are enabled
      if (read->IsAtLeastAcquire() && write->IsAtLeastRelease() &&
          // TODO: check if this is correct:
          // this case no need to consider, because we have sc edges instead
          !(read->IsSeqCst() && write->IsSeqCst())) {
        return RelSeq(read, write, write, {write});
      }
      return std::nullopt;
    }

    // Note: for non-acquire reads we do not need to consider release-sequences
    if (!read->IsAtLeastAcquire()) {
      return std::nullopt;
    }

    // Otherwise we traverse through the rf edges backwards:
    Event* rf = read;
    std::vector<Event*> releaseHeads;
    while (rf != nullptr) {
      rf = rf->GetReadFromEvent();
      assert((rf->IsWrite() || rf->IsModifyRMW()) &&
             "RF event must be a write or modifying rmw");
      if (rf->IsAtLeastRelease()) {
        // We need to sync hb-clocks with this event
        releaseHeads.push_back(rf);
      }
      if (!rf->IsRMW()) {
        // Found a 1st write event before which there should be only writes in
        // the same thread. Basically, we found the `lastWrite` parameter of the
        // `RelSeq`.
        break;
      }
      if (rf->IsAcqRel()) {
        // we found the event which might be a part of another release-sequence
        // and it has done all the necessary synchronization (if required), so
        // we can complete the current release sequence here and sync just with
        // this rel-acq rmw event
        return RelSeq(read, rf, rf, releaseHeads);
      }
    }

    Event* lastWrite = rf;
    assert(lastWrite != nullptr &&
           "The end of the RMW chain must be a valid write event");
    if (lastWrite->IsAtLeastRelease()) {
      // already pushed to the release heads + this rel seq cannot be broken,
      // because we have the following sequence:
      // W_rel -> RMW -> ... -> RMW -> R_acq
      //   A   ->  R1 -> ... ->  Rn -> B
      // which means that there could not be any other write event between `A`
      // and `R1`.
      return RelSeq(read, lastWrite, lastWrite, releaseHeads);
    }

    // Now we traverse the events in the thread of lastWrite to find the most
    // recent release write event
    int threadId = lastWrite->threadId;
    auto it = std::ranges::find_if(
        eventsPerThread[threadId].rbegin(), eventsPerThread[threadId].rend(),
        [this, lastWrite](EventId eventId) -> bool {
          Event* event = events[eventId];
          return (event->IsWrite() || event->IsModifyRMW()) &&
                 event->IsAtLeastRelease() &&
                 event->location == lastWrite->location;
        });

    if (it == eventsPerThread[threadId].rend()) {
      // No release write found, so we cannot establish any release sequence,
      // so we return nullopt
      return std::nullopt;
    }

    // Now we need to ensure that detected release sequence is not broken
    // already. So we search for any write event that is located between
    // `releaseWrite` and `lastWrite` in mo-graph. If there is such event, then
    // release sequence is broken and we return nullopt.
    // We don't need to check the same thing in the RMW chain, because RMW
    // events when reading from one-another will be directly mo-before the next
    // one, so no other event will be mo-between them.
    Event* releaseWrite = events[*it];
    // append the found release write to the event with which read should sync
    // with
    releaseHeads.push_back(releaseWrite);
    RelSeq currentRelSeq(read, releaseWrite, lastWrite, releaseHeads);
    if (IsRelSeqValid(currentRelSeq)) {
      // No write event found that is mo-between `releaseWrite` and `lastWrite`,
      // so we can establish the release sequence.
      // Note: it does not mean that later during execution such event will not
      // appear, if it appears we will abort the execution and mark it as
      // infeasible, but for now we just optimistically treat rel-seq as valid.
      return currentRelSeq;
    }
    // TODO: check if we need to sync with releaseHeads of the current release
    //       sequence even if it is broken

    // Release sequence is broken, so we return nullopt
    return std::nullopt;
  }

  // Checks that passed release sequence is valid, i.e. right now there is no
  // such write event that breaks it.
  // Note: at some later point in the execution of a program new mo-edges
  // might be added to the graph that could break existing release-sequences, so
  // we account for that throughout the code.
  bool IsRelSeqValid(const RelSeq& rs) const {
    Event* releaseWrite = rs.releaseWrite;
    Event* lastWrite = rs.lastWrite;
    int threadId = lastWrite->threadId;

    for (int tid = 0; tid < nThreads; ++tid) {
      if (tid == threadId) continue;  // skip thread of [release/last]Write
      auto& tidEvents = eventsPerThread[tid];
      for (auto rit = tidEvents.rbegin(); rit != tidEvents.rend(); ++rit) {
        EventId eid = *rit;
        Event* event = events[eid];
        if (!(event->IsWrite() || event->IsModifyRMW())) continue;
        if (event->location != lastWrite->location) continue;

        // event -mo-> releaseWrite
        if (IsMoReachable(event, releaseWrite)) {
          // all more recent writes in this thread are mo-before the
          // `releaseWrite` so we can check the next thread
          break;
        }

        // releaseWrite -mo-> event -mo-> lastWrite
        if (IsMoReachable(releaseWrite, event) &&
            IsMoReachable(event, lastWrite)) {
          // release sequence is broken, so we return nullopt
          return false;
        }
      }
    }

    return true;
  }

  // Checks if any established release sequence got broken
  bool HasBrokenRelSeq() const {
    for (const auto& rs : establishedRelSeqs) {
      if (!IsRelSeqValid(rs)) {
        log() << "Broken release sequence:\n" << rs.AsString() << "\n";
        Print(log());
        return true;
      }
    }
    return false;
  }

  bool IsMoReachable(Event* from, Event* to) const {
    assert(from != nullptr && "From event must be non-null");
    assert(to != nullptr && "To event must be non-null");
    assert(from->location == to->location &&
           "From and to events must be at the same location");
    // Use an explicit stack for DFS to check if "to" is reachable from "from"
    // via only mo edges
    if (from == to) return true;

    std::unordered_set<EventId> visited;
    std::vector<Event*> stack;
    stack.push_back(from);

    while (!stack.empty()) {
      Event* curr = stack.back();
      stack.pop_back();
      if (curr == to) {
        return true;
      }
      if (visited.count(curr->id)) continue;
      visited.insert(curr->id);

      for (EdgeId eid : curr->edges) {
        const auto& edge = edges[eid];
        if (edge.type == EdgeType::MO) {
          if (events[edge.to]->location != to->location) {
            std::cerr << "MO edges must be established between events with the "
                         "same location, but got: \n\t" +
                             events[edge.from]->AsString() + " --mo-->\n\t" +
                             events[edge.to]->AsString()
                      << std::endl;
            assert(false &&
                   "MO edges must be established between events with the same "
                   "location");
          }
          if (!visited.count(edge.to)) {
            stack.push_back(events[edge.to]);
          }
        }
      }
    }
    return false;
  }

  // ===== Methods to create general graph edges =====

  // Creates a po-edge between last event in the same thread as `event`
  void CreatePoEdgeToEvent(Event* event) {
    int threadId = event->threadId;
    EventId eventId = event->id;

    // connect prev event in the same thread with new event via PO edge
    auto lastEventInSameThread = events[eventsPerThread[threadId].back()];
    AddEdge(EdgeType::PO, lastEventInSameThread->id, eventId);

    // update last event in thread
    eventsPerThread[threadId].push_back(eventId);

    // insert in all-events vector
    events.push_back(event);

    // set correct hb-clocks for new event
    event->clock = lastEventInSameThread->clock;
    event->clock.Increment(threadId);
  }

  // Adds an sc-edge between prev sc-write (to the same location as `event`)
  // and `event`
  void CreateScEdgeToEvent(Event* event) {
    assert(event->IsSeqCst() && "Event must be an SC access");

    // last seq_cst write should appear before us
    EventId lastSeqCstWriteEvent = GetLastSeqCstWriteEventId(event->location);
    if (lastSeqCstWriteEvent != -1) {
      auto lastSeqCstWrite = events[lastSeqCstWriteEvent];
      assert((lastSeqCstWrite->IsWrite() || lastSeqCstWrite->IsModifyRMW()) &&
             "Prev scq-cst event must be a write or modifying rmw");
      AddEdge(EdgeType::SC, lastSeqCstWrite->id, event->id);

      // unite current hb-clock with last seq-cst write
      event->clock.UniteWith(lastSeqCstWrite->clock);
    }
  }

  // ===== Methods to create mo edges =====

  // Applies Seq-Cst Write-Read Coherence rules: establishes mo-edge between
  // last sc-write to the same location and `write` event
  // W'_x --sc--> R_x     W'_x --sc--> R_x
  //               ^       \            ^
  //               |        \           |
  //               rf  =>    \          rf
  //               |          \         |
  //              W_x          --mo--> W_x
  void CreateSeqCstReadWriteCoherenceEdges(Event* write, Event* read) {
    // TODO: in all such assertions do I need to check for correct RMW type?
    assert(write->IsWriteOrRMW() && read->IsReadOrRMW() &&
           "Write and read events must be of correct type");
    assert(write->location == read->location &&
           "Write and read events must be of the same location");
    assert(read->GetReadFromEvent() == write &&
           "Read event must read-from the write event");

    EventId lastSeqCstWriteEvent = GetLastSeqCstWriteEventId(read->location);
    // no such event, no need to create mo edge
    if (lastSeqCstWriteEvent == -1) return;

    // create mo-edge between last sc-write and `write` that `read` event
    // reads-from
    auto lastSeqCstWrite = events[lastSeqCstWriteEvent];
    assert((lastSeqCstWrite->IsWrite() || lastSeqCstWrite->IsModifyRMW()) &&
           "Last sc-write event must be a write or modifying rmw");
    assert(lastSeqCstWrite->location == read->location &&
           "Last sc-write event must have the same location as read event");

    if (lastSeqCstWrite->id == write->id)
      return;  // no need to create edge to itself

    // TODO: check that sc-edge between lastSeqCstWrite and read exists
    AddEdge(EdgeType::MO, lastSeqCstWrite->id, write->id);
  }

  // Applies Write-Read Coherence rules: establishes mo-edges between
  // stores (which happened-before `read`) and `write`
  // W'_x --hb--> R_x     W'_x --hb--> R_x
  //               ^       \            ^
  //               |        \           |
  //               rf  =>    \          rf
  //               |          \         |
  //              W_x          --mo--> W_x
  void CreateWriteReadCoherenceEdges(Event* write, Event* read) {
    assert(write->IsWriteOrRMW() && read->IsReadOrRMW() &&
           "Write and read events must be of correct type");
    assert(write->location == read->location &&
           "Write and read events must be of the same location");
    assert(read->GetReadFromEvent() == write &&
           "Read event must read-from the write event");

    IterateThroughMostRecentEventsByPredicate(
        [write, read](Event* otherEvent) -> bool {
          return (read->id != otherEvent->id &&
                  write->id != otherEvent->id &&  // not the same events
                  (otherEvent->IsWrite() || otherEvent->IsModifyRMW()) &&
                  otherEvent->location == read->location &&
                  otherEvent->HappensBefore(read));
        },
        [this, write](Event* otherEvent) -> void {
          // establish mo edge
          AddEdge(EdgeType::MO, otherEvent->id, write->id);
        });
  }

  // Applies Read-Read Coherence rules: establishes mo-edges between
  // `write` and write events from which other read events
  // (which happened-before `read`) reaf-from.
  // W'_x --rf--> R'_x     W'_x --rf--> R'_x
  //               |         |           |
  //               hb  =>    mo          hb
  //               |         |           |
  //               v         v           v
  // W_x  --rf--> R_x      W_x  --rf--> R_x
  void CreateReadReadCoherenceEdges(Event* write, Event* read) {
    assert(write->IsWriteOrRMW() && read->IsReadOrRMW() &&
           "Write and read events must be of correct type");
    assert(write->location == read->location &&
           "Write and read events must be of the same location");
    assert(read->GetReadFromEvent() == write &&
           "Read event must read-from the write event");

    IterateThroughMostRecentEventsByPredicate(
        [write, read](Event* otherEvent) -> bool {
          return (read->id != otherEvent->id &&
                  write->id != otherEvent->id &&  // not the same events
                  otherEvent->IsReadOrRMW() &&
                  otherEvent->location == read->location &&
                  otherEvent->HappensBefore(read) &&
                  otherEvent->GetReadFromEvent() != nullptr &&
                  otherEvent->GetReadFromEvent() !=
                      write  // R'_x does not read-from `write`
          );
        },
        [this, write](Event* otherRead) -> void {
          auto otherWrite = otherRead->GetReadFromEvent();
          // establish mo-edge
          AddEdge(EdgeType::MO, otherWrite->id, write->id);
        });
  }

  // TODO: instead of sc-edges, add reads-from from "Repairing Sequential
  // Consistency in C/C++11"?
  //
  // Applies Seq-Cst / MO Consistency rules:
  // establishes mo-edge between last sc-write and current sc-write-event if
  // their locations match W'_x --sc--> W_x  =>  W'_x --mo--> W_x
  void CreateSeqCstConsistencyEdges(Event* event) {
    assert(event->IsWriteOrRMW() && event->IsSeqCst() &&
           "Event must be a write/rmw with seq-cst order");

    EventId lastSeqCstWriteEvent = GetLastSeqCstWriteEventId(event->location);
    if (lastSeqCstWriteEvent == -1) return;
    auto lastSeqCstWrite = events[lastSeqCstWriteEvent];
    assert((lastSeqCstWrite->IsWrite() || lastSeqCstWrite->IsModifyRMW()) &&
           "Last sc-write event must be a write or modifying rmw");
    assert(lastSeqCstWrite->location == event->location &&
           "Last sc-write event must have the same location as event");
    // TODO: check that sc-edge between lastSeqCstWrite and read exists
    AddEdge(EdgeType::MO, lastSeqCstWrite->id, event->id);
  }

  // Applies Write-Write Coherence rules: establishes mo-edges between
  // other writes that happened before `event` only for the same location
  // W'_x --hb--> W_x  =>  W'_x --mo--> W_x
  void CreateWriteWriteCoherenceEdges(Event* event) {
    assert(event->IsWriteOrRMW());

    IterateThroughMostRecentEventsByPredicate(
        [event](Event* otherEvent) -> bool {
          return (event->id != otherEvent->id &&  // not the same event
                  (otherEvent->IsWrite() || otherEvent->IsModifyRMW()) &&
                  otherEvent->location == event->location &&  // same location
                  otherEvent->HappensBefore(event));
        },
        [this, event](Event* otherEvent) -> void {
          // establish mo edge
          AddEdge(EdgeType::MO, otherEvent->id, event->id);
        });
  }

  // Applies Read-Write Coherence rules: establishes mo-edges between
  // stores that are read-from by reads which happened-before our write
  // `event` W'_x --rf--> R'_x      W'_x --rf--> R'_x
  //               |         \            |
  //               hb   =>    \           hb
  //               |           \          |
  //               v            \         v
  //              W_x            --mo--> W_x
  void CreateReadWriteCoherenceEdges(Event* event) {
    assert(event->IsWriteOrRMW());

    IterateThroughMostRecentEventsByPredicate(
        [event](Event* otherEvent) -> bool {
          return (event->id != otherEvent->id &&  // not the same event
                  otherEvent->IsReadOrRMW() &&
                  otherEvent->location == event->location &&  // same location
                  otherEvent->HappensBefore(event));
        },
        [this, event](Event* otherEvent) -> void {
          assert(otherEvent->GetReadFromEvent() != nullptr &&
                 "Read event must have read-from event");
          auto writeEvent = otherEvent->GetReadFromEvent();
          // establish mo edge
          AddEdge(EdgeType::MO, writeEvent->id, event->id);
        });
  }

  // Applies RMW Consitency rules: establishes mo-edge between
  // store that is read-from by the current RMW event and the RMW event
  // itself. W_x --rf--> RMW_x  =>   W_x --mo--> RMW_x
  void CreateRmwConsistencyEdges(Event* write, Event* rmw) {
    assert(write->IsWriteOrRMW() && rmw->IsModifyRMW() &&
           "Write and RMW events must be of correct type");
    assert(write->location == rmw->location &&
           "Write and RMW events must be of the same location");
    assert(rmw->GetReadFromEvent() == write &&
           "RMW event must read-from the Write event");

    // establish mo-edge
    AddEdge(EdgeType::MO, write->id, rmw->id);
  }

  // Applies RMW Atomicity rules: establishes mo-edges between
  // RMW event and all writes that are mo-after `write` event
  // (from which RMW reads-from)
  // RMW_x <--rf-- W_x     RMW_x <--rf-- W_x
  //                |        \            |
  //                mo   =>   \           mo
  //                |          \          |
  //                v           \         v
  //              W'_x           --mo--> W'_x
  void CreateRmwAtomicityEdges(Event* write, Event* rmw) {
    assert(write->IsWriteOrRMW() && rmw->IsModifyRMW() &&
           "Write and RMW events must be of correct type");
    assert(write->location == rmw->location &&
           "Write and RMW events must be of the same location");
    assert(rmw->GetReadFromEvent() == write &&
           "RMW event must read-from the Write event");

    for (EdgeId edgeId : write->edges) {
      Edge& edge = edges[edgeId];
      if (edge.type == EdgeType::MO && edge.to != rmw->id) {
        // establish mo-edge
        AddEdge(EdgeType::MO, rmw->id, edge.to);
      }
    }
  }

  // ===== Helper methods =====

  template <class Predicate, class Callback>
    requires requires(Predicate p, Event* event) {
      { p(event) } -> std::same_as<bool>;
    } && requires(Callback cb, Event* event) {
      { cb(event) } -> std::same_as<void>;
    }
  void IterateThroughMostRecentEventsByPredicate(Predicate&& predicate,
                                                 Callback&& callback) {
    // iterate through each thread
    for (int t = 0; t < nThreads; ++t) {
      // iterate from most recent to earliest events in thread `t`
      const auto& threadEvents = eventsPerThread[t];
      for (auto it = threadEvents.rbegin(); it != threadEvents.rend(); ++it) {
        Event* otherEvent = events[*it];
        if (!std::forward<Predicate>(predicate)(otherEvent)) continue;
        // invoke `callback` on the most recent event in the thread `t`
        std::forward<Callback>(callback)(otherEvent);
        break;  // no need to invoke `callback` with earlier events from this
                // thread
      }
    }
  }

  EventId GetLastSeqCstWriteEventId(int location) const {
    if (lastSeqCstWriteEvents.contains(location)) {
      return lastSeqCstWriteEvents.at(location);
    }
    return -1;
  }

  void AddEdge(EdgeType type, EventId from, EventId to) {
    // for mo edges we might add duplicates, so we need to check that such
    // mo-edge does not exist
    if (type == EdgeType::MO && ExistsEdge(type, from, to)) {
      // log() << "Edge already exists: " << events[from]->AsString() << "
      // --" << WmmUtils::EdgeTypeToString(type) << "--> "
      //           << events[to]->AsString() << "\n";
      return;
    }

    auto& from_edges = events[from]->edges;
    EdgeId eId = edges.size();
    Edge e = {eId, type, from, to};
    edges.push_back(e);
    from_edges.push_back(eId);

    if (inSnapshotMode) {
      snapshotEdges.insert(eId);
    }
  }

  bool ExistsEdge(EdgeType type, EventId from, EventId to) const {
    const auto& from_edges = events[from]->edges;
    auto it =
        std::ranges::find_if(from_edges, [this, from, to, type](EdgeId eId) {
          auto& edge = edges[eId];
          return edge.from == from && edge.to == to && edge.type == type;
        });
    return it != from_edges.end();
  }

  // Check execution graph for consistency createria:
  //  * modification order is acyclic
  bool IsConsistent() {
    // TODO: should consistency criteria be taken from paper "Repairing
    // Sequential Consistency in C/C++11"?
    enum { NOT_VISITED = 0, IN_STACK = 1, VISITED = 2 };
    std::vector<int> colors(events.size(),
                            NOT_VISITED);  // each event is colored 0 (not
                                           // visited), 1 (entered), 2 (visited)
    std::vector<std::pair<Event*, bool /* already considered */>> stack;

    for (auto e : events) {
      assert(colors[e->id] != IN_STACK &&
             "Should not be possible, invalid cycle detection");
      if (colors[e->id] == VISITED) continue;
      stack.push_back({e, false});

      while (!stack.empty()) {
        auto [event, considred] = stack.back();
        EventId eventId = event->id;

        stack.pop_back();
        if (considred) {
          colors[eventId] = VISITED;
          continue;
        }
        stack.push_back({event, true});  // next time we take it out, we do
                                         // not traverse its edges

        for (auto edgeId : event->edges) {
          Edge& edge = edges[edgeId];
          if (edge.type != EdgeType::MO) continue;
          if (colors[edge.to] == NOT_VISITED) {
            stack.push_back({events[edge.to], false});
            colors[edge.to] = IN_STACK;
          } else if (colors[edge.to] == IN_STACK) {
            // cycle detected
            return false;
          }
        }
      }
    }

    return true;
  }

  void StartSnapshot() {
    assert(!inSnapshotMode && "Snapshot started twice");
    inSnapshotMode = true;
  }

  void ApplySnapshot() {
    assert(inSnapshotMode && "Applying snapshot not in snapshot mode");
    inSnapshotMode = false;
    snapshotEdges.clear();
  }

  void DiscardSnapshot() {
    assert(inSnapshotMode && "Discarding snapshot not in snapshot mode");
    // clearing all added edges from the graph
    // TODO: make sure below 'note' is true
    // Note: all appended edges will be in the suffixes of all edges arrays
    // 1. removing from edges vector
    while (!edges.empty() && snapshotEdges.contains(edges.back().id)) {
      edges.pop_back();
    }

    // 2. removing from events edges
    for (auto event : events) {
      auto& eventEdges = event->edges;
      while (!eventEdges.empty() && snapshotEdges.contains(eventEdges.back())) {
        eventEdges.pop_back();
      }
    }

    // reset snapshot state
    inSnapshotMode = false;
    snapshotEdges.clear();
  }

  void Clean() {
    establishedRelSeqs.clear();
    edges.clear();
    for (auto event : events) {
      delete event;
    }
    events.clear();
    eventsPerThread.clear();
    lastSeqCstWriteEvents.clear();
  }

  void InitThreads(int nThreads) {
    this->nThreads = nThreads;
    eventsPerThread.resize(nThreads);

    // insert dummy events (with all-zero hbClocks),
    // which will be the first event in each thread
    for (int t = 0; t < nThreads; ++t) {
      // TODO: DummyEvents are all ?seq-cst? (now rlx) writes, do I need to
      // add proper ?sc?-egdes between them? For now I don't
      int eventId = events.size();
      auto dummyEvent = new DummyEvent(eventId, nThreads, t);
      events.push_back(dummyEvent);
      eventsPerThread[t].push_back(eventId);
    }
  }

  std::vector<Edge> edges;
  std::vector<Event*> events;
  std::vector<std::vector<EventId>> eventsPerThread;
  std::map<int /* location */, EventId> lastSeqCstWriteEvents;
  std::vector<RelSeq> establishedRelSeqs;
  std::unordered_set<EdgeId>
      snapshotEdges;  // edges that are part of the snapshot (which case be
                      // discarded or applied, which is usefull when adding
                      // rf-edge)
  std::mt19937 gen{std::random_device{}()};  // random number generator for
                                             // randomized rf-edge selection
  bool inSnapshotMode = false;
  int nThreads = 0;
  // TODO: add a global flag to enable/disable this feature
  bool fullReleaseSequenceSupportEnabled = true;
};

}  // namespace ltest::wmm