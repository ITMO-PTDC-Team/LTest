#pragma once

#include <vector>

#include "../logger.h"
#include "event.h"

namespace ltest::wmm {

// Represents a release-sequence:
// Release sequence consists of:
// - read or rmw event with at least acquire memory order
// - the chain of rmw events each reading from one another (their chain could be
// obtained by traversing the rf edges backwards from the `read` event)
// - `releaseWrite` event which is in the same thread as `lastWrite` and all
// write events to the same location between them as well. This write is the
// most recent in the thread with release memory order, might be null
// - `lastWrite` event is the write from which the chain of rmw events starts to
// read from, might be null
// - `releaseHeads` write events in the whole sequence which are at least
// release, these are all the events with which vector clock `read` must
// synchronize with
//
// Example:
// releaseWrite_rel -> ... -> lastWrite_rlx
//                                  \-> RMW1_rel
//                                         \-> RMW2_rlx
//                                               \-> read_acq
// releaseHeads: [releaseWrite_rel, RMW1_rel]
struct RelSeq {
  // TODO: check all asserts are meaningful and valid
  RelSeq(Event* read, Event* releaseWrite, Event* lastWrite,
         std::vector<Event*> releaseHeads)
      : read(read),
        releaseWrite(releaseWrite),
        lastWrite(lastWrite),
        releaseHeads(releaseHeads) {
#ifdef DEBUG
    log() << "Creating RelSeq: " << AsString() << "\n";
#endif

    assert(read != nullptr && "Read event must be non-null");
    assert(read->IsReadOrRMW() && "Read event must be of correct type");
    assert(read->IsAtLeastAcquire() && "Read event must be at least acquire");

    // TODO: now we create RelSeq only it fully builds, but we might have
    // multiple release heads with no releaseWrite, check if such case should
    // also produce the RelSeq
    assert(releaseWrite != nullptr && "Release write event must be non-null");
    assert(releaseWrite->IsWriteOrRMW() &&
           "Release write event must be of correct type");
    // TODO: in case if fences are absent, we do not support them right now
    assert(releaseWrite->IsAtLeastRelease() &&
           "Release write event must be at least release");
    assert(read->location == releaseWrite->location &&
           "Read and release write events must be at the same location");

    // TODO: check for correctness of `lastWrite != nullptr` as well
    assert(lastWrite != nullptr && "Last write event must be non-null");
    assert(releaseWrite->threadId == lastWrite->threadId &&
           "Release write and last write events must be in the same thread");

    assert(!releaseHeads.empty() && "Release heads must be non-empty");
    for (auto* head : releaseHeads) {
      assert(head != nullptr && "Release head must be non-null");
      assert(head->IsWriteOrRMW() && "Release head must be of correct type");
      assert(head->IsAtLeastRelease() &&
             "Release head must be at least release");
      assert(head->location == read->location &&
             "Release head must be at the same location as the read event");
    }
  }

  Event* read;
  Event* releaseWrite;
  Event* lastWrite;
  std::vector<Event*> releaseHeads;

  int releaseWriteThreadId() const {
    assert(releaseWrite != nullptr && "Release write event must be non-null");
    return releaseWrite->threadId;
  }

  std::string AsString() const {
    std::stringstream ss;

    ss << "RelSeq: \n";
    ss << "\treleaseWrite: " << releaseWrite->AsString() << "\n";
    ss << "\tlastWrite: " << lastWrite->AsString() << "\n";
    ss << "\treleaseHeads: [\n";
    for (size_t i = 0; i < releaseHeads.size(); ++i) {
      ss << "\t\t" << releaseHeads[i]->AsString() << "\n";
    }
    ss << "\t]\n";
    ss << "\tread: " << read->AsString() << "\n";

    return ss.str();
  }
};

}  // namespace ltest::wmm