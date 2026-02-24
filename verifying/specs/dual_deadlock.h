#pragma once
#include "../../runtime/include/lincheck_dual.h"
#include "../../runtime/include/value_wrapper.h"

namespace spec {

struct DualDeadlock {
  void RequestWait(int /*op_id*/) {}

  std::optional<ValueWrapper> FollowUpWait(int /*op_id*/) {
    return std::nullopt; // never ready
  }

  static auto GetDualMethods() {
    using S = DualDeadlock;
    DualMethodMap<S> m;

    DualRequestMethod<S> req = [](S* s, void*, int op_id) { s->RequestWait(op_id); };
    DualFollowUpMethod<S> fol = [](S* s, void*, int op_id) { return s->FollowUpWait(op_id); };

    m.emplace("wait", DualBlockingMethod<S>{req, fol});
    return m;
  }
};

}  // namespace spec