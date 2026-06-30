#pragma once
#include <gflags/gflags.h>

#include <chrono>
#include <memory>
#include <stdexcept>
#include <type_traits>

#include "blocking_primitives.h"
#include "custom_round.h"
#include "latomic.h"
#include "lib.h"
#include "lincheck_dual.h"
#include "lincheck_recursive.h"
#include "logger.h"
#include "pct_strategy.h"
#include "pretty_print.h"
#include "random_strategy.h"
#include "round_robin_strategy.h"
#include "scheduler.h"
#include "strategy_verifier.h"
#include "verifying_macro.h"
#include "workload_policy.h"

namespace ltest {

enum StrategyType { RR, RND, TLA, PCT };

constexpr const char *GetLiteral(StrategyType t) {
  switch (t) {
    case RR:
      return "rr";
    case RND:
      return "random";
    case TLA:
      return "tla";
    case PCT:
      return "pct";
  }
  return "";
}

constexpr const char *GetLiteral(DeadlockPolicy p) {
  switch (p) {
    case DeadlockPolicy::Fail:
      return "fail";
    case DeadlockPolicy::Check:
      return "checker";
    case DeadlockPolicy::Explore:
      return "explore";
    case DeadlockPolicy::Rollback:
      return "rollback";
  }
  return "";
}

class NoOverride {};
struct DefaultCanceler {
  static void Cancel() {};
};

namespace detail {

template <class S>
concept HasWorkloadPolicy = requires {
  { S::GetWorkloadPolicy() } -> std::same_as<ltest::WorkloadPolicy>;
};

template <class LinearSpec>
using DefaultVerifierForSpec =
    std::conditional_t<HasWorkloadPolicy<LinearSpec>,
                       ReservePolicyVerifier<LinearSpec>,
                       DefaultStrategyTaskVerifier>;

template <class Spec>
concept HasSpecCreateTarget = requires {
  { Spec::CreateTarget() }
      -> std::same_as<std::unique_ptr<typename Spec::target_obj_t>>;
};

template <class Spec>
concept HasLinearSpecCreateTarget = requires {
  { Spec::linear_spec_t::CreateTarget() }
      -> std::same_as<std::unique_ptr<typename Spec::target_obj_t>>;
};

template <class Spec>
std::function<std::unique_ptr<typename Spec::target_obj_t>()>
MakeTargetFactory() {
  using TargetObj = typename Spec::target_obj_t;
  if constexpr (HasSpecCreateTarget<Spec>) {
    return []() { return Spec::CreateTarget(); };
  } else if constexpr (HasLinearSpecCreateTarget<Spec>) {
    return []() { return Spec::linear_spec_t::CreateTarget(); };
  } else {
    return []() { return std::make_unique<TargetObj>(); };
  }
}

}  // namespace detail

template <class TargetObj, class LinearSpec,
          class LinearSpecHash = std::hash<LinearSpec>,
          class LinearSpecEquals = std::equal_to<LinearSpec>,
          class OptionsOverride = NoOverride, class Canceler = DefaultCanceler>
struct Spec {
  using target_obj_t = TargetObj;
  using linear_spec_t = LinearSpec;
  using linear_spec_hash_t = LinearSpecHash;
  using linear_spec_equals_t = LinearSpecEquals;
  using options_override_t = OptionsOverride;
  using cancel_t = Canceler;
  using verifier_t = detail::DefaultVerifierForSpec<linear_spec_t>;
};

template <class TargetObj, class LinearSpec,
          class LinearSpecHash = std::hash<LinearSpec>,
          class LinearSpecEquals = std::equal_to<LinearSpec>,
          class OptionsOverride = NoOverride, class Canceler = DefaultCanceler>
struct SpecDual {
  using target_obj_t = TargetObj;
  using linear_spec_t = LinearSpec;
  using linear_spec_hash_t = LinearSpecHash;
  using linear_spec_equals_t = LinearSpecEquals;
  using options_override_t = OptionsOverride;
  using cancel_t = Canceler;
  using verifier_t = detail::DefaultVerifierForSpec<linear_spec_t>;
};

struct Opts {
  size_t threads;
  size_t tasks;
  size_t switches;
  size_t rounds;
  bool minimize;
  size_t exploration_runs;
  size_t minimization_runs;
  bool wmm_enabled;
  // Depth (only for TLA)
  size_t depth;
  bool forbid_all_same;
  bool verbose;
  bool syscall_trap;
  StrategyType typ;
  std::vector<int> thread_weights;
  DeadlockPolicy deadlock_policy;
  size_t seed;
};

struct DefaultOptions {
  size_t threads;
  size_t tasks;
  size_t switches;
  size_t rounds;
  size_t depth;
  bool forbid_all_same;
  bool verbose;
  const char *strategy;
  const char *weights;
  size_t minimization_runs;
  size_t exploration_runs;
  bool wmm_enabled;
  size_t seed = 0;
  const char *deadlock_policy = GetLiteral(DeadlockPolicy::Fail);
};

void SetOpts(const DefaultOptions &def);
Opts ParseOpts();
std::vector<std::string> split(const std::string &s, char delim);
DeadlockPolicy DeadlockPolicyFromLiteral(std::string a);

template <typename TargetObj, StrategyTaskVerifier Verifier>
std::unique_ptr<Strategy> MakeStrategy(
    Opts &opts, std::vector<TaskBuilder> l,
    std::function<std::unique_ptr<TargetObj>()> target_factory) {
  switch (opts.typ) {
    case RR: {
      std::cout << "round-robin\n";
      if (opts.seed != 0) {
        std::cout << "seed     = " << opts.seed << "\n";
      }
      return std::make_unique<RoundRobinStrategy<TargetObj, Verifier>>(
          opts.threads, std::move(l), std::move(target_factory), opts.seed);
    }
    case RND: {
      std::cout << "random\n";
      if (opts.seed != 0) {
        std::cout << "seed     = " << opts.seed << "\n";
      }
      std::vector<int> weights = opts.thread_weights;
      if (weights.empty()) {
        weights.assign(opts.threads, 1);
      }
      if (weights.size() != opts.threads) {
        throw std::invalid_argument{
            "number of threads not equal to number of weights"};
      }
      return std::make_unique<RandomStrategy<TargetObj, Verifier>>(
          opts.threads, std::move(l), std::move(weights),
          std::move(target_factory), opts.seed);
    }
    case PCT: {
      std::cout << "pct\n";
      if (opts.seed != 0) {
        std::cout << "seed     = " << opts.seed << "\n";
      }
      return std::make_unique<PctStrategy<TargetObj, Verifier>>(opts.threads,
                                                                std::move(l),
                                                                std::move(target_factory),
                                                                opts.seed);
    }
    default:
      assert(false && "unexpected type");
      throw std::invalid_argument{"unexpected strategy type"};
  }
}

// Keeps pointer to strategy to pass reference to base scheduler.
template <StrategyTaskVerifier Verifier>
struct StrategySchedulerWrapper : StrategyScheduler<Verifier> {
  StrategySchedulerWrapper(std::unique_ptr<Strategy> strategy_ptr,
                           ModelChecker& checker,
                           std::vector<CustomRound> custom_rounds,
                           PrettyPrinter& pretty_printer, size_t max_tasks,
                           size_t max_rounds, bool minimize,
                           size_t exploration_runs, size_t minimization_runs,
                           DeadlockPolicy deadlock_policy)
      : StrategyScheduler<Verifier>(*strategy_ptr.get(), checker,
                                    std::move(custom_rounds), pretty_printer,
                                    max_tasks, max_rounds, minimize,
                                    exploration_runs, minimization_runs,
                                    deadlock_policy),
        strategy(std::move(strategy_ptr)) {}

 private:
  std::unique_ptr<Strategy> strategy;
};

template <typename TargetObj, StrategyTaskVerifier Verifier>
std::unique_ptr<Scheduler> MakeScheduler(ModelChecker &checker, Opts &opts,
                                         const std::vector<TaskBuilder> &l,
                                         std::vector<CustomRound> custom_rounds,
                                         PrettyPrinter &pretty_printer,
                                         const std::function<void()> &cancel,
                                         std::function<std::unique_ptr<TargetObj>()>
                                             target_factory) {
  std::cout << "strategy = ";
  switch (opts.typ) {
    case RR:
    case PCT:
    case RND: {
      auto strategy = MakeStrategy<TargetObj, Verifier>(
          opts, std::move(l), std::move(target_factory));
      auto scheduler = std::make_unique<StrategySchedulerWrapper<Verifier>>(
          std::move(strategy), checker, std::move(custom_rounds),
          pretty_printer, opts.tasks, opts.rounds, opts.minimize,
          opts.exploration_runs, opts.minimization_runs, opts.deadlock_policy);
      return scheduler;
    }
    case TLA: {
      std::cout << "tla\n";
      auto scheduler = std::make_unique<TLAScheduler<TargetObj, Verifier>>(
          opts.tasks, opts.rounds, opts.threads, opts.switches, opts.depth,
          std::move(l), checker, pretty_printer, cancel,
          std::move(target_factory));
      return scheduler;
    }
    default: {
      assert(false && "Unknown strategy type specified");
    }
  }
}

inline int TrapRun(std::unique_ptr<Scheduler> &&scheduler,
                   PrettyPrinter &pretty_printer) {
  ltest::ClearTestFailure();
  auto result = scheduler->Run();
  if (ltest::HasTestFailure()) {
    std::cout << ltest::GetTestFailureMessage() << "\n";
    return 1;
  }
  if (result.has_value()) {
    if (result->reason == Scheduler::NonLinearizableHistory::Reason::DEADLOCK) {
      std::cout << "deadlock detected:\n";
      pretty_printer.PrettyPrint(
          result->seq, scheduler->GetStartegyThreadsCount(), std::cout);
      (void)scheduler.release();
      return 4;
    } else if (result->reason == Scheduler::NonLinearizableHistory::Reason::
                                     NON_LINEARIZABLE_HISTORY) {
      std::cout << "non linearized:\n";
      pretty_printer.PrettyPrint(
          result->seq, scheduler->GetStartegyThreadsCount(), std::cout);
      (void)scheduler.release();
      return 3;
    } else {
      std::abort();
    }
  } else {
    std::cout << "success!\n";
    return 0;
  }
}

// ---- dual runner ----

// A checker stub for: always "passes".
struct NoopDualChecker final : DualModelChecker {
  bool Check(const std::vector<DualHistoryEvent> &) override { return true; }
};

template <StrategyTaskVerifier Verifier>
struct DualStrategySchedulerWrapper : DualStrategyScheduler<Verifier> {
  DualStrategySchedulerWrapper(std::unique_ptr<Strategy> strategy_ptr,
                               DualModelChecker& checker,
                               PrettyPrinter& pretty_printer,
                               size_t max_tasks, size_t max_rounds,
                               bool minimize,
                               size_t exploration_runs,
                               size_t minimization_runs,
                               DeadlockPolicy deadlock_policy)
      : DualStrategyScheduler<Verifier>(*strategy_ptr.get(),
                                        checker,
                                        pretty_printer,
                                        max_tasks,
                                        max_rounds,
                                        minimize,
                                        exploration_runs,
                                        minimization_runs,
                                        deadlock_policy),
        strategy(std::move(strategy_ptr)) {}

 private:
  std::unique_ptr<Strategy> strategy;
};

template <typename TargetObj, StrategyTaskVerifier Verifier>
std::unique_ptr<DualScheduler> MakeDualScheduler(
    DualModelChecker &checker, Opts &opts, const std::vector<TaskBuilder> &l,
    PrettyPrinter &pretty_printer,
    std::function<std::unique_ptr<TargetObj>()> target_factory) {
  std::cout << "strategy = " << GetLiteral(opts.typ) << "\n";
  // We support RR/RND/PCT the same way as normal.
  switch (opts.typ) {
    case RR:
    case PCT:
    case RND: {
      auto strategy = MakeStrategy<TargetObj, Verifier>(
          opts, std::move(l), std::move(target_factory));
      return std::make_unique<DualStrategySchedulerWrapper<Verifier>>(
          std::move(strategy), checker, pretty_printer, opts.tasks, opts.rounds,
          opts.minimize, opts.exploration_runs, opts.minimization_runs,
          opts.deadlock_policy);
    }
    case TLA: {
      return std::make_unique<DualTLAScheduler<TargetObj, Verifier>>(
          opts.tasks, opts.rounds, opts.threads, opts.switches, opts.depth,
          std::move(l), checker, pretty_printer, opts.deadlock_policy,
          std::move(target_factory));
    }
    default:
      assert(false);
  }
}

inline int TrapRunDual(std::unique_ptr<DualScheduler> &&scheduler,
                       PrettyPrinter &pretty_printer) {
  auto result = scheduler->Run();
  if (result.has_value()) {
    if (result->reason ==
        DualScheduler::NonLinearizableHistory::Reason::DEADLOCK) {
      std::cout << "deadlock detected:\n";
      pretty_printer.PrettyPrint(
          result->seq, scheduler->GetStartegyThreadsCount(), std::cout);
      (void)scheduler.release();
      return 4;
    } else if (result->reason == DualScheduler::NonLinearizableHistory::Reason::
                                     NON_LINEARIZABLE_HISTORY) {
      std::cout << "non linearized:\n";
      pretty_printer.PrettyPrint(
          result->seq, scheduler->GetStartegyThreadsCount(), std::cout);
      (void)scheduler.release();
      return 3;
    } else {
      std::abort();
    }
  } else {
    std::cout << "success!\n";
    return 0;
  }
}

template <class Spec,
          StrategyTaskVerifier Verifier = typename Spec::verifier_t>
int Run(int argc, char *argv[], std::vector<CustomRound> custom_rounds = {}) {
  if constexpr (!std::is_same_v<typename Spec::options_override_t,
                                ltest::NoOverride>) {
    SetOpts(Spec::options_override_t::GetOptions());
  }
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  Opts opts = ParseOpts();

  logger_init(opts.verbose);
  std::cout << "verbose: " << std::boolalpha << opts.verbose << "\n";
  std::cout << "threads  = " << opts.threads << "\n";
  std::cout << "tasks    = " << opts.tasks << "\n";
  std::cout << "switches = " << opts.switches << "\n";
  std::cout << "rounds   = " << opts.rounds << "\n";
  std::cout << "minimize = " << std::boolalpha << opts.minimize << "\n";
  std::cout << "deadlock_policy = " << GetLiteral(opts.deadlock_policy) << "\n";
  if (opts.minimize) {
    std::cout << "exploration runs = " << opts.exploration_runs << "\n";
    std::cout << "minimization runs = " << opts.minimization_runs << "\n";
  }
  std::cout << "wmm enabled = " << std::boolalpha << opts.wmm_enabled << "\n";
  std::cout << "targets  = " << task_builders.size() << "\n";

  PrettyPrinter pretty_printer;

  using lchecker_t =
      LinearizabilityCheckerRecursive<typename Spec::linear_spec_t,
                                      typename Spec::linear_spec_hash_t,
                                      typename Spec::linear_spec_equals_t>;
  lchecker_t checker{Spec::linear_spec_t::GetMethods(),
                     typename Spec::linear_spec_t{}};
  auto target_factory = detail::MakeTargetFactory<Spec>();

  auto scheduler = MakeScheduler<typename Spec::target_obj_t, Verifier>(
      checker, opts, std::move(task_builders), std::move(custom_rounds),
      pretty_printer, &Spec::cancel_t::Cancel, std::move(target_factory));
  std::cout << "\n\n";
  std::cout.flush();
  return TrapRun(std::move(scheduler), pretty_printer);
}

template <class SpecDual,
          StrategyTaskVerifier Verifier = typename SpecDual::verifier_t>
int RunDual(int argc, char *argv[]) {
  if constexpr (!std::is_same_v<typename SpecDual::options_override_t,
                                ltest::NoOverride>) {
    SetOpts(SpecDual::options_override_t::GetOptions());
  }
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  Opts opts = ParseOpts();

  logger_init(opts.verbose);
  std::cout << "verbose: " << std::boolalpha << opts.verbose << "\n";
  std::cout << "threads  = " << opts.threads << "\n";
  std::cout << "tasks    = " << opts.tasks << "\n";
  std::cout << "switches = " << opts.switches << "\n";
  std::cout << "rounds   = " << opts.rounds << "\n";
  std::cout << "minimize = " << std::boolalpha << opts.minimize << "\n";
  std::cout << "deadlock_policy = " << GetLiteral(opts.deadlock_policy) << "\n";
  if (opts.minimize) {
    std::cout << "exploration runs = " << opts.exploration_runs << "\n";
    std::cout << "minimization runs = " << opts.minimization_runs << "\n";
  }
  std::cout << "targets  = " << task_builders.size() << "\n";

  PrettyPrinter pretty_printer;

  using dual_checker_t =
      LinearizabilityDualCheckerRecursive<
          typename SpecDual::linear_spec_t,
          typename SpecDual::linear_spec_hash_t,
          typename SpecDual::linear_spec_equals_t>;
  dual_checker_t checker{SpecDual::linear_spec_t::GetDualMethods(),
                         typename SpecDual::linear_spec_t{}};
  auto target_factory = detail::MakeTargetFactory<SpecDual>();
  auto scheduler = MakeDualScheduler<typename SpecDual::target_obj_t, Verifier>(
      checker, opts, std::move(task_builders), pretty_printer,
      std::move(target_factory));
  std::cout << "\n\n";
  std::cout.flush();
  return TrapRunDual(std::move(scheduler), pretty_printer);
}

}  // namespace ltest

// `...` is used instead of named argument in order to allow
// user to specify custom rounds without wrapping them into
// parenthesis `()` manually
#define LTEST_ENTRYPOINT(spec_obj_t, ...)                                    \
  int main(int argc, char *argv[]) {                                         \
    std::vector<CustomRound> custom_rounds;                                  \
    __VA_OPT__(std::vector<std::vector<std::vector<TaskBuilder>>> builders = \
                   {__VA_ARGS__};                                            \
               for (auto &v : builders) {                                    \
                 custom_rounds.emplace_back(std::move(v));                   \
               })                                                            \
    return ltest::Run<spec_obj_t>(argc, argv, std::move(custom_rounds));     \
  }

#define LTEST_ENTRYPOINT_CONSTRAINT(spec_obj_t, strategy_verifier, ...)      \
  int main(int argc, char *argv[]) {                                         \
    std::vector<CustomRound> custom_rounds;                                  \
    __VA_OPT__(std::vector<std::vector<std::vector<TaskBuilder>>> builders = \
                   {__VA_ARGS__};                                            \
               for (auto &v : builders) {                                    \
                 custom_rounds.emplace_back(std::move(v));                   \
               })                                                            \
    return ltest::Run<spec_obj_t, strategy_verifier>(                        \
        argc, argv, std::move(custom_rounds));                               \
  }

#define LTEST_ENTRYPOINT_DUAL_CONSTRAINT(spec_obj_t, strategy_verifier) \
  int main(int argc, char *argv[]) {                                    \
    return ltest::RunDual<spec_obj_t, strategy_verifier>(argc, argv);   \
  }

#define LTEST_ENTRYPOINT_DUAL(spec_obj_t)          \
  int main(int argc, char *argv[]) {               \
    return ltest::RunDual<spec_obj_t>(argc, argv); \
  }
