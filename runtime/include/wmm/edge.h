#pragma once

#include "common.h"

namespace ltest::wmm {

struct Edge {
  EdgeId id;
  EdgeType type;
  EventId from;
  EventId to;
};

}  // namespace ltest::wmm