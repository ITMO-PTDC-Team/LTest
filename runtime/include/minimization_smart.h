#pragma once

#include <cassert>
#include <random>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "logger.h"
#include "minimization.h"

namespace ltest {

/**
 * `SmartMinimizor` uses a genetic algorithm to minimize the number of
 * tasks in a non-linearizable history.
 *
 * It tries to generate new `Solution` objects which contain non-linearizable
 * histories of the round with some tasks removed. Each `Solution` object has a
 * `fitness` value in range `[0.0, 1.0]`; the closer fitness is to 1.0, the
 * smaller the non-linearizable history is.
 *
 * During execution this minimizor keeps a population of `Solution` objects,
 * selects the best two solutions from the population, and generates new
 * offsprings by crossing and mutating their sets of removed tasks. The new
 * offsprings are added to the population and sorted by fitness. The best
 * solutions are preserved for the next generation.
 *
 * When the minimization budget is exhausted, the best solution is used.
 */
struct SmartMinimizor : public RoundMinimizor {
  SmartMinimizor() = delete;

  explicit SmartMinimizor(int exploration_runs, int minimization_runs,
                          int max_offsprings_per_generation = 5,
                          int offsprings_generation_attemps = 10,
                          int initial_mutations_count = 10)
      : exploration_runs(exploration_runs),
        minimization_runs(minimization_runs),
        max_offsprings_per_generation(max_offsprings_per_generation),
        offsprings_generation_attemps(offsprings_generation_attemps),
        mutations_count(initial_mutations_count) {
    std::random_device dev;
    rng = std::mt19937(dev());
  }

  void Minimize(CandidateRunner& sched,
                AttemptReport& nonlinear_histories) const override {
    // Reset minimizer state for this round.
    population.clear();
    exhausted_candidates.clear();
    population.insert(
        Solution(nonlinear_histories, nonlinear_histories.operations.size()));

    for (int r = 0; r < minimization_runs; ++r) {
      const Solution& p1 = *population.begin();
      const Solution& p2 =
          *(population.size() < 2 ? population.begin()
                                  : std::next(population.begin()));

      std::vector<Solution> offsprings =
          GenerateOffsprings(sched, p1, p2, nonlinear_histories);
      for (auto& s : offsprings) {
        population.insert(s);
      }

      while (population.size() > max_population_size) {
        population.erase(std::prev(population.end()));
      }
    }

    assert(!population.empty());
    const Solution& best_solution = *population.begin();

    auto request = AttemptRequest(best_solution.nonlinear_histories,
                                  AttemptKind::FixedReplay);
    auto replayed = ReplayRound(sched, request);
    if (replayed.has_value()) {
      nonlinear_histories = replayed.value();
      return;
    }

    log() << "[minimizer] final replay did not reproduce a meaningful bug; "
             "keeping original replayable history\n";
  }

 private:
  struct Solution {
    explicit Solution(const AttemptReport& histories, int total_tasks)
        : nonlinear_histories(histories) {
      size_t total_threads = 0;
      int valid_tasks = 0;

      // Save valid task ids per thread.
      for (const auto& operation : histories.operations) {
        total_threads = std::max(total_threads, operation.thread + 1);
        valid_tasks++;
        tasks[operation.thread].insert(operation.id);
      }

      // Cache the fitness value. The fewer tasks and threads are left, the
      // closer the fitness is to 1.0.
      float tasks_fitness =
          1.0f - (valid_tasks * 1.0f) / (total_tasks * 1.0f);
      float threads_fitness =
          eps + 1.0f - (tasks.size() * 1.0f) / (total_threads * 1.0f);

      assert(tasks_fitness >= 0.0f && tasks_fitness <= 1.0f);
      assert(threads_fitness >= 0.0f && threads_fitness <= 1.0f);

      fitness = tasks_fitness * threads_fitness;
    }

    float GetFitness() const { return fitness; }

    // ThreadId -> {ValidTaskId1, ValidTaskId2, ...}
    std::unordered_map<int, std::unordered_set<int>> tasks;
    AttemptReport nonlinear_histories;

   private:
    float eps = 0.0001f;
    float fitness = 0.0f;
  };

  struct SolutionSorter {
    bool operator()(const Solution& a, const Solution& b) const {
      // The biggest fitness goes first.
      return a.GetFitness() > b.GetFitness();
    }
  };

  // Mutation removes a single random task from a round.
  void DropRandomTask(
      std::unordered_map<int, std::unordered_set<int>>& threads) const {
    if (threads.empty()) return;

    int thread_index = std::uniform_int_distribution<int>(
        0, static_cast<int>(threads.size()) - 1)(rng);
    auto it = std::next(threads.begin(), thread_index);
    auto& tasks = it->second;

    if (tasks.empty() ||
        (tasks.size() == 1 && threads.size() == 2)) {
      return;
    }

    int task_index = std::uniform_int_distribution<int>(
        0, static_cast<int>(tasks.size()) - 1)(rng);
    auto task_it = std::next(tasks.begin(), task_index);
    tasks.erase(task_it);
  }

  // Generates offsprings by crossing and mutating the set of removed tasks in
  // the parents `p1` and `p2`.
  std::vector<Solution> GenerateOffsprings(
      CandidateRunner& sched, const Solution& p1, const Solution& p2,
      const AttemptReport& original_histories) const {
    assert(offsprings_generation_attemps > 0);
    std::vector<Solution> offsprings;

    for (int offspring = 1; offspring <= max_offsprings_per_generation;
         ++offspring) {
      int left_attempts = offsprings_generation_attemps;
      while (left_attempts--) {
        auto new_threads = CrossProduct(&p1, &p2);

        for (int m = 1; m <= mutations_count; ++m) {
          if (mutations_count > 1 || dist(rng) < 0.95) {
            DropRandomTask(new_threads);
          }
        }

        auto request =
            AttemptRequest(original_histories, AttemptKind::Reschedule);
        RemoveInvalidTasks(request, new_threads);

        std::vector<int> candidate_ids;
        for (const auto& operation : request.operations)
          candidate_ids.push_back(operation.id);
        if (exhausted_candidates.contains(candidate_ids)) continue;

        auto histories = ExploreRound(sched, request, exploration_runs);

        if (histories.has_value()) {
          offsprings.emplace_back(histories.value(),
                                  original_histories.operations.size());
          break;
        }
        // Spend the exploration budget once per unsuccessful operation set.
        exhausted_candidates.insert(std::move(candidate_ids));
      }
    }

    // Decrease the number of permitted mutations over time when too many
    // offspring generation attempts fail.
    if (static_cast<int>(offsprings.size()) * 2 < max_offsprings_per_generation &&
        mutations_count > 1) {
      mutations_count--;
    }

    return offsprings;
  }

  // Removes operations that do not appear in `valid_threads` from a candidate.
  void RemoveInvalidTasks(
      AttemptRequest& request,
      const std::unordered_map<int, std::unordered_set<int>>& valid_threads)
      const {
    std::unordered_set<int> removed;
    for (const auto& operation : request.operations) {
      auto thread = valid_threads.find(operation.thread);
      if (thread == valid_threads.end() ||
          !thread->second.contains(operation.id))
        removed.insert(operation.id);
    }
    RemoveOperations(request, removed);
  }

  /**
   * Crosses two parents `p1` and `p2` to generate a new set of threads with
   * tasks. If `p1` had `[p1:T1, p1:T2, p1:T3]` and `p2` had
   * `[p2:T1, p2:T2, p2:T3]`, a possible cross product is
   * `[p1:T1, p2:T2, p1:T3]`.
   *
   * The resulting number of threads is equal to
   * `min(p1.thread_count, p2.thread_count)`.
   */
  std::unordered_map<int, std::unordered_set<int>> CrossProduct(
      const Solution* p1, const Solution* p2) const {
    if (p1->tasks.size() >= p2->tasks.size()) {
      std::swap(p1, p2);
    }

    const float p = 0.5f;
    std::unordered_map<int, std::unordered_set<int>> new_threads;

    for (auto& [thread_id, task_ids] : p1->tasks) {
      if (p2->tasks.contains(thread_id) && dist(rng) >= p) {
        new_threads[thread_id] = p2->tasks.at(thread_id);
      } else {
        new_threads[thread_id] = task_ids;
      }
    }
    return new_threads;
  }

  const int exploration_runs;
  const int minimization_runs;
  // Max number of offsprings per generation.
  const int max_offsprings_per_generation;
  // Attempts to generate each offspring with a non-linearizable history.
  const int offsprings_generation_attemps;

  // Currently only the best and second-best solutions are used as parents, so
  // larger values do not improve the algorithm.
  const int max_population_size = 2;

  mutable int mutations_count;
  mutable std::multiset<Solution, SolutionSorter> population;
  mutable std::set<std::vector<int>> exhausted_candidates;
  mutable std::mt19937 rng;
  mutable std::uniform_real_distribution<double> dist{0.0, 1.0};
};

}  // namespace ltest
