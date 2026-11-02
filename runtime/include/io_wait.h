#pragma once

namespace ltest {

struct CoroBase;

// Use kernel descriptors with logical suspension during target execution.
// Check readiness and clear waits that can resume, without consuming events.
bool IsWaitingForIo(const CoroBase* task);
bool HasPendingExternalIo();

// Only external sources can make progress while all logical threads are
// blocked. False means the remaining internal waits cannot progress on their
// own. Fixed replay waits only for the selected task's external sources.
bool WaitForPendingIo(const CoroBase* task = nullptr);

}  // namespace ltest
