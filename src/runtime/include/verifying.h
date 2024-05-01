#pragma once
#include <memory>

#include "generators.h"
#include "lib.h"
#include "lincheck_recursive.h"
#include "logger.h"
#include "pretty_print.h"
#include "random_strategy.h"
#include "round_robin_strategy.h"
#include "scheduler.h"
#include "verifying_macro.h"

namespace ltest {

enum StrategyType { RR, RND, TLA };

template <class TargetObj, class LinearSpec,
          class LinearSpecHash = std::hash<LinearSpec>,
          class LinearSpecEquals = std::equal_to<LinearSpec>>
struct Spec {
  using target_obj_t = TargetObj;
  using linear_spec_t = LinearSpec;
  using linear_spec_hash_t = LinearSpecHash;
  using linear_spec_equals_t = LinearSpecEquals;
};

struct Opts {
  size_t threads;
  size_t tasks;
  size_t switches;
  size_t rounds;
  bool verbose;
  StrategyType typ;
  std::vector<int> thread_weights;
};

std::vector<std::string> parse_opts(std::vector<std::string> args, Opts &opts);

std::vector<std::string> split(const std::string &s, char delim);

template <typename TargetObj>
std::unique_ptr<Strategy> MakeStrategy(Opts &opts,
                                       std::vector<task_builder_t> l) {
  switch (opts.typ) {
    case RR: {
      return std::make_unique<RoundRobinStrategy<TargetObj>>(opts.threads,
                                                             std::move(l));
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
      return std::make_unique<RandomStrategy<TargetObj>>(
          opts.threads, std::move(l), std::move(weights));
    }
    default:
      assert(false && "unexpected typ");
  }
}

// Keeps pointer to strategy to pass reference to base scheduler.
// TODO: refactor.
struct StrategySchedulerWrapper : StrategyScheduler {
  StrategySchedulerWrapper(std::unique_ptr<Strategy> strategy,
                           ModelChecker &checker, size_t max_tasks,
                           size_t max_rounds, size_t threads_count)
      : strategy(std::move(strategy)),
        StrategyScheduler(*strategy.get(), checker, max_tasks, max_rounds,
                          threads_count){};

 private:
  std::unique_ptr<Strategy> strategy;
};

template <typename TargetObj>
std::unique_ptr<Scheduler> MakeScheduler(ModelChecker &checker, Opts &opts,
                                         std::vector<task_builder_t> l) {
  std::cout << "strategy = ";
  switch (opts.typ) {
    case RR:
    case RND: {
      auto strategy = MakeStrategy<TargetObj>(opts, std::move(l));
      auto scheduler = std::make_unique<StrategySchedulerWrapper>(
          std::move(strategy), checker, opts.tasks, opts.rounds, opts.threads);
      return scheduler;
    }
    case TLA: {
      std::cout << "tla\n";
      auto scheduler = std::make_unique<TLAScheduler<TargetObj>>(
          opts.tasks, opts.rounds, opts.threads, opts.switches, std::move(l),
          checker);
      return scheduler;
    }
  }
}

template <class Spec>
void Run(int argc, char *argv[]) {
  std::vector<std::string> args;
  for (size_t i = 1; i < argc; ++i) {
    args.push_back(std::string{argv[i]});
  }
  Opts opts;
  args = parse_opts(std::move(args), opts);

  logger_init(opts.verbose);
  std::cout << "threads  = " << opts.threads << "\n";
  std::cout << "tasks    = " << opts.tasks << "\n";
  std::cout << "switches = " << opts.switches << "\n";
  std::cout << "rounds   = " << opts.rounds << "\n";
  std::cout << "targets  = " << task_builders.size() << "\n";

  using lchecker_t =
      LinearizabilityCheckerRecursive<typename Spec::linear_spec_t,
                                      typename Spec::linear_spec_hash_t,
                                      typename Spec::linear_spec_equals_t>;
  lchecker_t checker{Spec::linear_spec_t::GetMethods(),
                     typename Spec::linear_spec_t{}};

  auto scheduler = MakeScheduler<typename Spec::target_obj_t>(
      checker, opts, std::move(task_builders));
  std::cout << "\n\n";
  std::cout.flush();

  auto result = scheduler->Run();
  if (result.has_value()) {
    std::cout << "non linearized:\n";
    pretty_print::pretty_print(result.value().second, std::cout, opts.threads);
  } else {
    std::cout << "success!\n";
  }
}

}  // namespace ltest

#define LTEST_ENTRYPOINT(spec_obj_t)         \
  namespace ltest {                          \
  std::vector<task_builder_t> task_builders; \
  GeneratedArgs gen_args = GeneratedArgs{};  \
  }                                          \
  int main(int argc, char *argv[]) {         \
    ltest::Run<spec_obj_t>(argc, argv);      \
    return 0;                                \
  }\
