#pragma once
#include <gflags/gflags.h>

#include <chrono>
#include <functional>
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
#include "tla_strategy.h"
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
          class OptionsOverride = NoOverride>
struct Spec {
  using target_obj_t = TargetObj;
  using linear_spec_t = LinearSpec;
  using linear_spec_hash_t = LinearSpecHash;
  using linear_spec_equals_t = LinearSpecEquals;
  using options_override_t = OptionsOverride;
  using verifier_t = detail::DefaultVerifierForSpec<linear_spec_t>;
};

template <class TargetObj, class LinearSpec,
          class LinearSpecHash = std::hash<LinearSpec>,
          class LinearSpecEquals = std::equal_to<LinearSpec>,
          class OptionsOverride = NoOverride>
struct SpecDual {
  using target_obj_t = TargetObj;
  using linear_spec_t = LinearSpec;
  using linear_spec_hash_t = LinearSpecHash;
  using linear_spec_equals_t = LinearSpecEquals;
  using options_override_t = OptionsOverride;
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
int RunStrategyRounds(const Opts &opts, Strategy &strategy,
                      const std::function<void()> &run,
                      const std::vector<CustomRound> &custom_rounds = {});
std::vector<std::string> split(const std::string &s, char delim);
DeadlockPolicy DeadlockPolicyFromLiteral(std::string a);

template <typename TargetObj, StrategyTaskVerifier Verifier>
std::unique_ptr<Strategy> MakeStrategy(
    Opts &opts, std::vector<TaskBuilder> l,
    std::function<std::unique_ptr<TargetObj>()> target_factory) {
  std::cout << "strategy = ";
  switch (opts.typ) {
    case RR: {
      std::cout << "round-robin\n";
      return std::make_unique<RoundRobinStrategy<TargetObj, Verifier>>(
          opts.threads, std::move(l), std::move(target_factory));
    }
    case RND: {
      std::cout << "random\n";
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
          std::move(target_factory));
    }
    case PCT: {
      std::cout << "pct\n";
      return std::make_unique<PctStrategy<TargetObj, Verifier>>(
          opts.threads, std::move(l), std::move(target_factory));
    }
    case TLA: {
      std::cout << "tla\n";
      return std::make_unique<TLAStrategy<TargetObj, Verifier>>(
          opts.threads, std::move(l), std::move(target_factory));
    }
    default:
      throw std::invalid_argument{"unexpected strategy type"};
  }
}

inline std::unique_ptr<Scheduler> MakeScheduler(ModelChecker &checker,
                                                const Opts &opts,
                                                Strategy &strategy,
                                                PrettyPrinter &pretty_printer) {
  return std::make_unique<StrategyScheduler>(strategy, checker, pretty_printer,
                                             opts.tasks, opts.deadlock_policy);
}

template <class Event>
int TrapRun(std::unique_ptr<BasicScheduler<Event>> &&scheduler,
            PrettyPrinter &pretty_printer) {
  ClearTestFailure();
  scheduler->Run();
  const auto &report = attempt_session->report;
  // Checked deadlocks remain in the report for parent-driven exploration.
  int result = report.result;
  if (result == Deadlock &&
      scheduler->GetDeadlockPolicy() != DeadlockPolicy::Fail) {
    result = Success;
  }
  if (attempt_session->request.kind != AttemptKind::Generate) return result;

  const size_t threads = scheduler->GetStrategyThreadsCount();
  switch (result) {
    case Success:
      std::cout << "success!\n";
      break;
    case AssertionFailure:
      std::cout << ltest::GetTestFailureMessage() << "\n";
      break;
    case NonLinearizable:
      std::cout << "non linearized:\n";
      pretty_printer.PrettyPrint(report, threads, std::cout);
      break;
    case Deadlock:
      std::cout << "deadlock detected:\n";
      pretty_printer.PrettyPrint(report, threads, std::cout);
      break;
    default:
      std::abort();
  }
  return result;
}

// ---- dual runner ----

inline std::unique_ptr<DualScheduler> MakeDualScheduler(
    DualLinearizabilityChecker &checker, const Opts &opts, Strategy &strategy,
    PrettyPrinter &pretty_printer) {
  return std::make_unique<DualStrategyScheduler>(
      strategy, checker, pretty_printer, opts.tasks, opts.deadlock_policy);
}

inline int TrapRunDual(std::unique_ptr<DualScheduler> &&scheduler,
                       PrettyPrinter &pretty_printer) {
  return TrapRun(std::move(scheduler), pretty_printer);
}

template <class Spec,
          StrategyTaskVerifier Verifier = typename Spec::verifier_t>
int Run(int argc, char *argv[], std::vector<CustomRound> custom_rounds = {}) {
  if constexpr (!std::is_same_v<typename Spec::options_override_t,
                                ltest::NoOverride>) {
    SetOpts(Spec::options_override_t::GetOptions());
  }
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  ltest_initialized = true;
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

  auto strategy = MakeStrategy<typename Spec::target_obj_t, Verifier>(
      opts, std::move(task_builders), detail::MakeTargetFactory<Spec>());
  auto run_once = [&] {
    PrettyPrinter pretty_printer;
    using lchecker_t =
        LinearizabilityCheckerRecursive<typename Spec::linear_spec_t,
                                        typename Spec::linear_spec_hash_t,
                                        typename Spec::linear_spec_equals_t>;
    lchecker_t checker{Spec::linear_spec_t::GetMethods(),
                       typename Spec::linear_spec_t{}};
    auto scheduler = MakeScheduler(checker, opts, *strategy, pretty_printer);
    TrapRun(std::move(scheduler), pretty_printer);
  };
  return RunStrategyRounds(opts, *strategy, run_once, custom_rounds);
}

template <class SpecDual,
          StrategyTaskVerifier Verifier = typename SpecDual::verifier_t>
int RunDual(int argc, char *argv[]) {
  if constexpr (!std::is_same_v<typename SpecDual::options_override_t,
                                ltest::NoOverride>) {
    SetOpts(SpecDual::options_override_t::GetOptions());
  }
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  ltest_initialized = true;
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

  auto strategy = MakeStrategy<typename SpecDual::target_obj_t, Verifier>(
      opts, std::move(task_builders), detail::MakeTargetFactory<SpecDual>());
  auto run_once = [&] {
    PrettyPrinter pretty_printer;
    using dual_checker_t = LinearizabilityDualCheckerRecursive<
        typename SpecDual::linear_spec_t, typename SpecDual::linear_spec_hash_t,
        typename SpecDual::linear_spec_equals_t>;
    dual_checker_t checker{SpecDual::linear_spec_t::GetDualMethods(),
                           typename SpecDual::linear_spec_t{}};
    auto scheduler =
        MakeDualScheduler(checker, opts, *strategy, pretty_printer);
    TrapRunDual(std::move(scheduler), pretty_printer);
  };
  return RunStrategyRounds(opts, *strategy, run_once);
}

}  // namespace ltest

// `...` is used instead of named argument in order to allow
// user to specify custom rounds without wrapping them into
// parenthesis `()` manually
#define LTEST_ENTRYPOINT(spec_obj_t, ...)                                    \
  int main(int argc, char *argv[]) {                                         \
    std::vector<ltest::CustomRound> custom_rounds;                           \
    __VA_OPT__(std::vector<std::vector<std::vector<ltest::TaskBuilder>>>      \
                   builders =                                                \
                   {__VA_ARGS__};                                            \
               for (auto &v : builders) {                                    \
                 custom_rounds.emplace_back(std::move(v));                   \
               })                                                            \
    return ltest::Run<spec_obj_t>(argc, argv, std::move(custom_rounds));     \
  }

#define LTEST_ENTRYPOINT_CONSTRAINT(spec_obj_t, strategy_verifier, ...)      \
  int main(int argc, char *argv[]) {                                         \
    std::vector<ltest::CustomRound> custom_rounds;                           \
    __VA_OPT__(std::vector<std::vector<std::vector<ltest::TaskBuilder>>>      \
                   builders =                                                \
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
