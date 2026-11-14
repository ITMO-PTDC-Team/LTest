#pragma once

#include <boost/context/stack_context.hpp>
#include <cstddef>

namespace ltest {

class DonorTls;

std::size_t GetFiberStackSize();

// Allocator copies live in Boost's fiber records. The donor and cached
// mappings outlive every record, including abandoned fibers, until _exit.
class FiberStackAllocator {
 public:
  FiberStackAllocator(DonorTls& donor, std::size_t bytes)
      : donor_(&donor), bytes_(bytes) {}

  boost::context::stack_context allocate();
  void deallocate(boost::context::stack_context& stack) noexcept;

 private:
  DonorTls* donor_;
  std::size_t bytes_;
};

namespace detail {
std::size_t FiberStackMappingSize(std::size_t bytes);
}

}  // namespace ltest
