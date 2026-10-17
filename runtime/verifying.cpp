#include "verifying.h"

#include <gflags/gflags.h>

#include <algorithm>
#include <cstddef>
#include <stdexcept>
#include <string>

#include "fork_round.h"
#include "minimization_smart.h"

namespace ltest {

namespace wmm {
bool wmm_enabled = false;
}  // namespace wmm

template <>
std::string toString<int>(const int& a) {
  return std::to_string(a);
}

template <>
std::string toString<size_t>(const size_t& a) {
  return std::to_string(a);
}

std::string toLower(std::string str) {
  std::transform(str.begin(), str.end(), str.begin(),
                 [](unsigned char c) { return std::tolower(c); });
  return str;
}

std::vector<std::string> split(const std::string& s, char delim) {
  std::vector<std::string> res{""};
  for (char c : s) {
    if (c == delim) {
      res.push_back("");
    } else {
      res.back() += c;
    }
  }
  return res;
}

StrategyType FromLiteral(std::string&& a) {
  if (a == GetLiteral(StrategyType::PCT)) {
    return StrategyType::PCT;
  } else if (a == GetLiteral(StrategyType::RND)) {
    return StrategyType::RND;
  } else if (a == GetLiteral(StrategyType::RR)) {
    return StrategyType::RR;
  } else if (a == GetLiteral(StrategyType::TLA)) {
    return StrategyType::TLA;
  } else {
    throw std::invalid_argument(a);
  }
}

DeadlockPolicy DeadlockPolicyFromLiteral(std::string a) {
  a = toLower(std::move(a));
  if (a == GetLiteral(DeadlockPolicy::Fail)) {
    return DeadlockPolicy::Fail;
  } else if (a == GetLiteral(DeadlockPolicy::Check)) {
    return DeadlockPolicy::Check;
  } else if (a == GetLiteral(DeadlockPolicy::Explore)) {
    return DeadlockPolicy::Explore;
  } else if (a == GetLiteral(DeadlockPolicy::Rollback)) {
    return DeadlockPolicy::Rollback;
  } else {
    throw std::invalid_argument(a);
  }
}

DEFINE_int32(threads, 2, "Number of threads");
DEFINE_int32(tasks, 15, "Number of tasks");
DEFINE_int32(switches, 100000000, "Number of switches");
DEFINE_int32(
    rounds, 5,
    "Number of rounds. When custom rounds are added, this value will show the "
    "number of generated rounds to run after all custom ones. This can be "
    "set to zero in case if you want to run only custom rounds");
DEFINE_bool(minimize, false, "Minimize nonlinear scenario");
DEFINE_int32(exploration_runs, 15,
             "Number of attempts to find nonlinearized round during each "
             "minimization step");
DEFINE_int32(minimization_runs, 15,
             "Number of minimization runs for smart minimizor");
DEFINE_bool(wmm_enabled, false,
            "Enable WMM graph usage (all atomic operations will be performed "
            "via this graph");
DEFINE_int32(depth, 0,
             "How many tasks can be executed on one thread(Only for TLA)");
DEFINE_bool(verbose, false, "Verbosity");
DEFINE_string(strategy, GetLiteral(StrategyType::RR), "Strategy");
DEFINE_string(weights, "", "comma-separated list of weights for threads");
DEFINE_string(deadlock_policy, GetLiteral(DeadlockPolicy::Fail),
              "Deadlock handling policy: fail, checker, explore, rollback.");
DEFINE_uint64(seed, 0, "Random seed; 0 uses std::random_device");

int RunStrategyRounds(const Opts& opts, Strategy& strategy,
                      const std::function<void()>& run,
                      const std::vector<CustomRound>& custom_rounds) {
  const size_t custom_attempts = custom_rounds.size() * opts.exploration_runs;
  auto& state = strategy.GetSharedState();
  const std::uint64_t seed = opts.seed ? opts.seed : std::random_device{}();
  state.rng.seed(seed);
  std::mt19937 c_seeds(seed);
  std::cout << "seed = " << seed << '\n';

  AttemptRequest generated;

  for (size_t round = 0; round < custom_attempts + opts.rounds; ++round) {
    const CustomRound* custom =
        round < custom_attempts ? &custom_rounds[round / opts.exploration_runs]
                                : nullptr;
    const unsigned c_seed = c_seeds();

    auto execute = [&](const AttemptRequest& request) {
      return RunForkAttempt(request, c_seed, [&] {
        // Auxiliary attempts are reported by the parent. Their child-local
        // typed histories/checkers are discarded when the worker exits.
        strategy.StartNextRound(custom);
        run();
      });
    };

    auto report = execute(generated);

    auto print_history = [&](const AttemptReport& history) {
      size_t threads = custom ? custom->threads.size() : opts.threads;
      for (const auto& op : history.operations)
        threads = std::max(threads, op.thread + 1);
      PrettyPrinter{}.PrettyPrint(history, threads, std::cout);
    };
    if (opts.verbose) print_history(report);

    std::unique_ptr<BaseStrategySharedState> search_state;
    CandidateRunner candidate =
        [&](const AttemptRequest& request) -> std::optional<AttemptReport> {
      if (request.operations.empty()) return std::nullopt;
      std::unordered_set<int> ids;
      for (const auto& op : request.operations) ids.insert(op.id);
      for (const auto& op : request.operations)
        if (op.id < 0 && !ids.contains(op.creator)) return std::nullopt;

      auto main_state = state.Snapshot();
      if (search_state) state.Restore(*search_state);
      auto result = execute(request);
      search_state = state.Snapshot();
      state.Restore(*main_state);

      if (result.result == Deadlock &&
          opts.deadlock_policy != DeadlockPolicy::Fail)
        result.result = Success;
      return result;
    };

    if (report.result == InfrastructureFailure) return InfrastructureFailure;
    const bool explore_deadlock =
        report.result == Deadlock &&
        (opts.deadlock_policy == DeadlockPolicy::Explore ||
         opts.deadlock_policy == DeadlockPolicy::Rollback);
    if (explore_deadlock) {
      report.result = Success;
      auto original = report;
      if (opts.deadlock_policy == DeadlockPolicy::Rollback) {
        for (auto it = original.operations.rbegin();
             it != original.operations.rend(); ++it) {
          if (it->progress_methods.empty()) continue;
          for (const auto& method : it->progress_methods) {
            auto builder = strategy.GetBuilderId(method);
            if (!builder) continue;
            auto request = AttemptRequest(original, AttemptKind::Reschedule);
            RemoveOperations(request, {it->id});
            int progress_id = 0;
            for (const auto& op : original.operations)
              progress_id = std::max(progress_id, op.id + 1);
            request.operations.emplace_back(progress_id, *builder, it->thread);
            for (size_t attempt = 0; attempt < opts.exploration_runs;
                 ++attempt) {
              auto result = candidate(request);
              if (result && result->result == InfrastructureFailure)
                return InfrastructureFailure;
              if (result && result->result) {
                report = std::move(*result);
                break;
              }
            }
            if (report.result) break;
          }
          // Match directed rollback's latest eligible blocked operation.
          break;
        }
      }
      for (size_t attempt = 0;
           attempt < opts.exploration_runs && !report.result; ++attempt) {
        auto result =
            candidate(AttemptRequest(original, AttemptKind::Reschedule));
        if (result && result->result == InfrastructureFailure)
          return InfrastructureFailure;
        if (result && result->result) {
          report = std::move(*result);
        }
      }
    }
    if (report.result == Deadlock &&
        opts.deadlock_policy == DeadlockPolicy::Check)
      report.result = Success;

    if (report.result) {
      if (opts.minimize) {
        auto reproduced =
            candidate(AttemptRequest(report, AttemptKind::FixedReplay));
        if (reproduced && reproduced->result == InfrastructureFailure)
          return InfrastructureFailure;
        if (reproduced && MeaningfulFailure(*reproduced)) {
          try {
            SameInterleavingMinimizor().Minimize(candidate, report);
            StrategyExplorationMinimizor(opts.exploration_runs)
                .Minimize(candidate, report);
            SmartMinimizor(opts.exploration_runs, opts.minimization_runs)
                .Minimize(candidate, report);
          } catch (const std::exception& error) {
            std::cerr << "LTest infrastructure failure: " << error.what()
                      << '\n';
            return InfrastructureFailure;
          }
          std::cout << "Minimized confirmed failure ("
                    << report.operations.size() << " operations):\n";
          print_history(report);
        } else {
          std::cerr << "Replay could not reproduce the failure; retaining "
                       "original history\n";
          print_history(report);
        }
      }
      std::cerr << "LTest verified target failure: " << report.result << '\n';
      if (!opts.minimize && explore_deadlock) print_history(report);
      return report.result;
    }

    if (strategy.IsExhausted()) break;
  }
  std::cout << "success!\n";
  return Success;
}

void SetOpts(const DefaultOptions& def) {
  FLAGS_threads = def.threads;
  FLAGS_tasks = def.tasks;
  FLAGS_switches = def.switches;
  FLAGS_rounds = def.rounds;
  FLAGS_depth = def.depth;
  FLAGS_verbose = def.verbose;
  FLAGS_strategy = def.strategy;
  FLAGS_weights = def.weights;
  FLAGS_exploration_runs = def.exploration_runs;
  FLAGS_minimization_runs = def.minimization_runs;
  FLAGS_wmm_enabled = def.wmm_enabled;
  FLAGS_seed = def.seed;
  FLAGS_deadlock_policy =
      (def.deadlock_policy != nullptr && def.deadlock_policy[0] != '\0')
          ? def.deadlock_policy
          : GetLiteral(DeadlockPolicy::Fail);
}

// Extracts required opts, returns the rest of args.
Opts ParseOpts() {
  auto opts = Opts();
  opts.threads = FLAGS_threads;
  opts.tasks = FLAGS_tasks;
  opts.switches = FLAGS_switches;
  opts.rounds = FLAGS_rounds;
  opts.deadlock_policy = DeadlockPolicyFromLiteral(FLAGS_deadlock_policy);
  opts.seed = static_cast<size_t>(FLAGS_seed);
  opts.minimize = FLAGS_minimize;
  opts.exploration_runs = FLAGS_exploration_runs;
  opts.minimization_runs = FLAGS_minimization_runs;
  opts.wmm_enabled = wmm_enabled = FLAGS_wmm_enabled;
  opts.verbose = FLAGS_verbose;
  opts.typ = FromLiteral(std::move(FLAGS_strategy));
  opts.depth = FLAGS_depth;
  std::vector<int> thread_weights;
  if (FLAGS_weights != "") {
    auto splited = split(FLAGS_weights, ',');
    thread_weights.reserve(splited.size());
    for (auto& s : splited) {
      thread_weights.push_back(std::stoi(s));
    }
  }
  opts.thread_weights = std::move(thread_weights);
  return opts;
}

}  // namespace ltest
