#pragma once
// Boost context has stack unwinding if the stack isn't terminated properly/
// However this is not acceptable due the possible dependencies in stacks
// destructors
#include <boost/context/detail/config.hpp>
#include <boost/context/detail/disable_overload.hpp>
#include <boost/context/detail/exception.hpp>
#include <boost/context/detail/fcontext.hpp>
#include <boost/context/fixedsize_stack.hpp>
#include <cstddef>
#include <memory>
#include <utility>

#define private public
#include <boost/context/fiber.hpp>
#undef private

namespace ltest::context {

// Reclaim detached fibers only after every task in the round has terminated.
// A detached fiber can still be referenced from another suspended fiber's
// on-stack control record until that point.
std::size_t FreeForgottenFiberStacks() noexcept;

inline void forget(boost::context::fiber& context) noexcept {
  context.fctx_ = nullptr;
}

boost::context::stack_context AllocateFiberStack();
void DeallocateFiberStack(boost::context::stack_context&) noexcept;

class stack_allocator {
 public:
  boost::context::stack_context allocate() { return AllocateFiberStack(); }
  void deallocate(boost::context::stack_context& context) noexcept {
    DeallocateFiberStack(context);
  }
};

class fiber_context {
 public:
  fiber_context() noexcept = default;

  template <typename Fn, typename = boost::context::detail::disable_overload<
                             fiber_context, Fn>>
  explicit fiber_context(Fn&& fn)
      : context_(std::allocator_arg, stack_allocator{},
                 [fn = std::forward<Fn>(fn)](
                     boost::context::fiber&& context) mutable {
                   return std::move(fn(Adopt(std::move(context)))).release();
                 }) {}
  fiber_context(fiber_context&&) noexcept = default;
  fiber_context& operator=(fiber_context&&) noexcept = default;
  fiber_context(const fiber_context&) = delete;
  fiber_context& operator=(const fiber_context&) = delete;
  fiber_context resume() && {
    return fiber_context{std::move(context_).resume()};
  }
  static fiber_context Adopt(boost::context::fiber&& context) noexcept {
    return fiber_context{std::move(context)};
  }
  void forget() noexcept { context::forget(context_); }
  static std::size_t FreeForgottenStacks() noexcept {
    return FreeForgottenFiberStacks();
  }
  explicit operator bool() const noexcept {
    return static_cast<bool>(context_);
  }
  bool operator!() const noexcept { return !context_; }

 private:
  boost::context::fiber release() noexcept { return std::move(context_); }
  explicit fiber_context(boost::context::fiber&& context) noexcept
      : context_(std::move(context)) {}
  boost::context::fiber context_;
};

}  // namespace ltest::context
