#include "include/attempt.h"

namespace ltest {
bool MeaningfulFailure(const AttemptReport& report) {
  if (report.result != AssertionFailure && report.result != NonLinearizable &&
      report.result != Deadlock)
    return false;

  std::unordered_set<int> started, finished;
  for (auto& e : report.events) {
    if (e.IsStart())
      started.insert(e.operation);
    else if (e.IsFinish())
      finished.insert(e.operation);
  }

  if (started.empty()) return false;
  if (report.result != Deadlock) return true;
  for (int id : started)
    if (!finished.contains(id)) return true;
  return false;
}

void RemoveOperations(AttemptRequest& request,
                      const std::unordered_set<int>& removed) {
  std::erase_if(request.operations,
                [&](const auto& op) { return removed.contains(op.id); });
  std::erase_if(request.ordering, [&](int id) { return removed.contains(id); });
}

AttemptSession* attempt_session = nullptr;

}  // namespace ltest
