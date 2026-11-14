#include "include/fiber_stack.h"

#include <boost/context/protected_fixedsize_stack.hpp>
#include <boost/context/stack_traits.hpp>
#include <limits>
#include <new>

#include "context/donor_tls.h"
#include "include/coro_ctx_guard.h"

namespace ltest {
namespace {

struct FreeStack {
  boost::context::stack_context context;
  FreeStack* next;
};

// Only the executor accesses the pool. Store links in free stack pages so
// returning a completed stack cannot allocate or throw.
FreeStack* free_stacks = nullptr;

}  // namespace

std::size_t detail::FiberStackMappingSize(std::size_t bytes) {
  const auto page = boost::context::stack_traits::page_size();
  if (!bytes || bytes > std::numeric_limits<std::size_t>::max() - 2 * page)
    throw std::bad_alloc();
  return ((bytes + page - 1) / page + 1) * page;
}

boost::context::stack_context FiberStackAllocator::allocate() {
  SchedCtxGuard scheduler;
  if (auto stack = donor_->AcquireStack(bytes_); stack.sp) return stack;

  const auto size = detail::FiberStackMappingSize(bytes_);
  for (auto** entry = &free_stacks; *entry; entry = &(*entry)->next) {
    if ((*entry)->context.size != size) continue;
    auto* node = *entry;
    auto stack = node->context;
    *entry = node->next;
    node->~FreeStack();
    return stack;
  }
  return boost::context::protected_fixedsize_stack(bytes_).allocate();
}

void FiberStackAllocator::deallocate(
    boost::context::stack_context& stack) noexcept {
  SchedCtxGuard scheduler;
  if (donor_->ReleaseStack(stack)) return;

  auto* storage = static_cast<char*>(stack.sp) - stack.size +
                  boost::context::stack_traits::page_size();
  free_stacks = new (storage) FreeStack{stack, free_stacks};
}

}  // namespace ltest
