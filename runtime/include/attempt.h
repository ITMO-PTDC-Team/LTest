#pragma once

#include <functional>
#include <optional>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lincheck_dual.h"
#include "value_wrapper.h"

namespace ltest {

enum class AttemptKind { Generate, FixedReplay, Reschedule };

enum ExitCode {
  Success = 0,
  AssertionFailure = 1,
  InfrastructureFailure = 2,
  NonLinearizable = 3,
  Deadlock = 4,
  ExpectedFailureNotFound = 5,
};

struct OperationDescription {
  OperationDescription() = default;
  OperationDescription(int id, std::uint64_t builder, size_t thread,
                       size_t creation_step = 0, int creator = -1)
      : id(id),
        builder(builder),
        thread(thread),
        creation_step(creation_step),
        creator(creator) {}

  int id = 0;  // pthread tasks use the negated simulated thread ID
  std::uint64_t builder = 0;
  size_t thread = 0;
  size_t creation_step = 0;
  int creator = -1;  // creating operation for a pthread task (id < 0)
  std::vector<std::string> progress_methods;
};

struct EventDescription {
  EventDescription() = default;
  EventDescription(int operation, DualHistoryEventKind kind,
                   ValueWrapper result = {})
      : operation(operation), kind(kind), result(std::move(result)) {}

  int operation;
  DualHistoryEventKind kind;
  ValueWrapper result;  // child-local; never serialized
  std::string text;

  [[nodiscard]] bool IsStart() const {
    return kind == DualHistoryEventKind::OrdinaryInvoke ||
           kind == DualHistoryEventKind::RequestInvoke;
  }

  [[nodiscard]] bool IsFinish() const {
    return kind == DualHistoryEventKind::OrdinaryResponse ||
           kind == DualHistoryEventKind::FollowUpResponse;
  }
};

struct AttemptHistory {
  std::vector<OperationDescription> operations;
  std::vector<int> ordering;
};

struct AttemptRequest : AttemptHistory {
  AttemptRequest() = default;
  AttemptRequest(const AttemptHistory& history, AttemptKind kind)
      : AttemptHistory(history), kind(kind) {}

  AttemptKind kind = AttemptKind::Generate;
};

struct AttemptReport : AttemptHistory {
  int result = Success;  // exit code retained with the history
  std::vector<EventDescription> events;
};

using CandidateRunner =
    std::function<std::optional<AttemptReport>(const AttemptRequest&)>;

bool MeaningfulFailure(const AttemptReport&);
void RemoveOperations(AttemptRequest&, const std::unordered_set<int>& removed);

// Worker-local execution context.
struct AttemptSession {
  const AttemptRequest& request;
  AttemptReport& report;

  [[nodiscard]] const OperationDescription* Source(int id) const {
    for (auto& op : request.operations)
      if (op.id == id) return &op;
    return nullptr;
  }
};
extern AttemptSession* attempt_session;

}  // namespace ltest
