#pragma once
#include <valgrind/memcheck.h>

#include <boost/context/detail/fcontext.hpp>
#include <boost/context/fiber.hpp>
#include <boost/context/fiber_fcontext.hpp>
#include <cassert>
#include "value_wrapper.h"
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#define panic() assert(false)

struct CoroBase;

// Current executing coroutine.
extern std::shared_ptr<CoroBase> this_coro;

extern boost::context::fiber_context sched_ctx;

// Runtime token.
// Target method could use token generator.
struct Token {
  // Parks the task. Yields.
  void Park();
  // Unpark the task parked by token.
  void Unpark();

 private:
  // Resets the token.
  void Reset();
  // If token is parked.
  bool parked{};

  friend class CoroBase;
};

extern "C" void CoroYield();

struct CoroBase : public std::enable_shared_from_this<CoroBase> {
  CoroBase(const CoroBase&) = delete;
  CoroBase(CoroBase&&) = delete;
  CoroBase& operator=(CoroBase&&) = delete;

  // Restart the coroutine from the beginning passing this_ptr as this.
  // Returns restarted coroutine.
  virtual std::shared_ptr<CoroBase> Restart(void* this_ptr) = 0;

  // Resume the coroutine to the next yield.
  void Resume();

  // Check if the coroutine is returned.
  bool IsReturned() const;

  // Returns return value of the coroutine.
  virtual value_wrapper GetRetVal() const;

  // Returns the name of the coroutine.
  virtual std::string_view GetName() const;

  // Returns the args as strings.
  virtual std::vector<std::string> GetStrArgs() const = 0;

  // Returns raw pointer to the tuple arguments.
  virtual void* GetArgs() const = 0;

  // Returns new pointer to the coroutine.
  // https://en.cppreference.com/w/cpp/memory/enable_shared_from_this
  std::shared_ptr<CoroBase> GetPtr();

  // Terminate the coroutine.
  void Terminate();

  // Sets the token.
  void SetToken(std::shared_ptr<Token>);

  // Checks if the coroutine is parked.
  bool IsParked() const;

  virtual ~CoroBase();

 protected:
  CoroBase() = default;

  friend void CoroBody(int);
  friend void ::CoroYield();

  template <typename Target, typename... Args>
  friend class Coro;

  // Return value.
  value_wrapper ret;
  // Is coroutine returned.
  bool is_returned{};
  // Name.
  std::string_view name;
  // Token.
  std::shared_ptr<Token> token{};
  boost::context::fiber_context ctx;
};

template <typename Target, typename... Args>
struct Coro final : public CoroBase {
  // CoroF is a target class method.
  using CoroF = std::function<value_wrapper(Target*, Args...)>;
  // ArgsToStringF converts arguments to the strings for pretty printing.
  using ArgsToStringsF =
      std::function<std::vector<std::string>(std::shared_ptr<void>)>;

  // unsafe: caller must ensure that this_ptr points to Target.
  std::shared_ptr<CoroBase> Restart(void* this_ptr) override {
    /**
     *  The task must be returned if we want to restart it.
     *   We can't just Terminate() it because it is the runtime responsibility
     * to decide, in which order the tasks should be terminated.
     *
     */
    assert(IsReturned());
    auto coro = New(func, this_ptr, args, args_to_strings, name);
    if (token != nullptr) {
      coro->token = std::move(token);
    }
    return coro;
  }

  // unsafe: caller must ensure that this_ptr points to Target.
  static std::shared_ptr<CoroBase> New(CoroF func, void* this_ptr,
                                       std::shared_ptr<void> args,
                                       ArgsToStringsF args_to_strings,
                                       std::string_view name) {
    auto c = std::make_shared<Coro>();
    c->func = std::move(func);
    c->args = std::move(args);
    c->name = name;
    c->args_to_strings = std::move(args_to_strings);
    c->this_ptr = this_ptr;
    c->ctx =
        boost::context::fiber_context([c](boost::context::fiber_context&& ctx) {
          auto real_args =
              reinterpret_cast<std::tuple<Args...>*>(c->args.get());
          auto this_arg =
              std::tuple<Target*>{reinterpret_cast<Target*>(c->this_ptr)};
          c->ret = std::apply(c->func, std::tuple_cat(this_arg, *real_args));
          c->is_returned = true;
          return std::move(ctx);
        });
    return c;
  }

  std::vector<std::string> GetStrArgs() const override {
    assert(args_to_strings != nullptr);
    return args_to_strings(args);
  }

  void* GetArgs() const override { return args.get(); }

 private:
  // Function to execute.
  CoroF func;
  // Pointer to the arguments, points to the std::tuple<Args...>.
  std::shared_ptr<void> args;
  // Function that can make strings from args for pretty printing.
  std::function<std::vector<std::string>(std::shared_ptr<void>)>
      args_to_strings;
  // Raw pointer to the target class object.
  void* this_ptr;
};

using Task = std::shared_ptr<CoroBase>;

// (this_ptr, thread_num) -> Task

struct TaskBuilder {
  using BuilderFunc = std::function<Task(void*, size_t)>;
  TaskBuilder(std::string name, BuilderFunc func)
      : name(name), builder_func(func) {}

  const std::string& GetName() const { return name; }

  Task Build(void* this_ptr, size_t thread_id) {
    return builder_func(this_ptr, thread_id);
  }

 private:
  std::string name;
  BuilderFunc builder_func;
};
