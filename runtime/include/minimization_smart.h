#pragma once

#include <random>
#include <set>
#include <unordered_map>
#include <vector>

#include "minimization.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"

/**
 * `SmartMinimizor` uses a genetic algorithm to minimize the number of tasks in
 * the nonlinearized history.
 *
 * It tries to generate new `Solution` objects which contain the nonlinearized
 * histories of the round with some tasks being removed. Each `Solution` object
 * has a `fitness` value in range `[0.0, 1.0]`, the closer fitness to 1.0, the
 * smaller the nonlinearizable history is.
 *
 * During execution this minimizor generates a population of `Solution` objects,
 * then it selects the best two solutions from the population and generates new
 * offsprings by crossing and mutating their set of removed tasks. The new
 * offsprings are added to the population, and sorted by their fitness values.
 * The best among them are preserved for the next offsprings generation.
 *
 * When the minimization budget is exhausted, the best solution is returned.
 */
struct SmartMinimizor : public RoundMinimizor {
  SmartMinimizor() = delete;
  explicit SmartMinimizor(int exploration_runs, int minimization_runs,
                          PrettyPrinter& pretty_printer,
                          // algorithm params
                          int max_offsprings_per_generation = 5,
                          int offsprings_generation_attemps = 10,
                          int initial_mutations_count = 10);

  void Minimize(
      SchedulerWithReplay& sched,
      Scheduler::NonLinearizableHistory& nonlinear_histories) const override;

 private:
  struct Solution {
    explicit Solution(const Strategy& strategy,
                      const Scheduler::NonLinearizableHistory& histories,
                      int total_tasks);

    float GetFitness() const;
    int GetValidTasks() const;

    std::unordered_map<int, std::unordered_set<int>>
        tasks;  // ThreadId -> { ValidTaskId1, ValidTaskId2, ... }
    Scheduler::NonLinearizableHistory nonlinear_histories;
    // Fitness is a value in range [0.0, 1.0], the bigger it is, the better is
    // the Solution.
   private:
    float eps = 0.0001;
    float fitness = 0.0;
    int valid_tasks = 0;
  };

  struct SolutionSorter {
    bool operator()(const Solution& a, const Solution& b) const {
      // the biggest fitness, goes first
      return a.GetFitness() > b.GetFitness();
    }
  };

  // Mutation basically removes a single random task from round.
  void DropRandomTask(
      std::unordered_map<int, std::unordered_set<int>>& threads) const;

  // Generates offsprings by crossing and mutating the set of removed tasks in
  // the parents `p1`, `p2`.
  std::vector<Solution> GenerateOffsprings(SchedulerWithReplay& sched,
                                           const Solution& p1,
                                           const Solution& p2) const;

  // Marks tasks as removed if they do not appear in `valid_threads`.
  void RemoveInvalidTasks(
      Strategy& strategy,
      const std::unordered_map<int, std::unordered_set<int>>& valid_threads)
      const;

  /**
   * Crosses two parents `p1` and `p2` to generate a new set of threads with
   * tasks. If `p1` had the following set of threads:
   *
   * `[p1:T1, p1:T2, p1:T3]`
   *
   * and `p2` had:
   *
   * `[p2:T1, p2:T2, p2:T3]`,
   *
   * possible crossproduct could look like this: `[p1:T1, p2:T2, p1:T3]` (a mix
   * of threads from both parents).
   *
   * Note: resulting number of threads will be equal to the
   * `min(p1.thread_count, p2.thread_count)`.
   */
  std::unordered_map<int, std::unordered_set<int>> CrossProduct(
      const Strategy& strategy, const Solution* p1, const Solution* p2) const;

  const int exploration_runs;
  const int minimization_runs;
  const int
      max_offsprings_per_generation;  // max number of offsprings per generation
  const int offsprings_generation_attemps;  // attemps to generate each
                                            // offspring with nonlinear history
  const int max_population_size =
      2;  // TODO: right now I only take best and second-best solutions from
          // population, so other values for `max_population_size` make no
          // sense. This should be somewhat fixed...
  mutable int mutations_count;
  mutable int total_tasks;
  mutable std::multiset<Solution, SolutionSorter> population;
  mutable std::mt19937 rng;
  mutable std::uniform_real_distribution<double> dist{0.0, 1.0};
  PrettyPrinter& pretty_printer;
};