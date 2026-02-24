#pragma once

#include <cassert>
#include <random>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "minimization.h"
#include "pretty_print.h"
#include "scheduler_fwd.h"

/**
 * SmartMinimizorT<Event> uses genetic algorithm to minimize number of tasks
 * in a non-linearizable history. Works for both HistoryEvent and DualHistoryEvent.
 */
template <class Event>
struct SmartMinimizorT : public RoundMinimizorT<Event> {
  using Base = RoundMinimizorT<Event>;
  using SchedulerT = typename Base::SchedulerT;
  using NonLinearizableHistoryT = typename Base::NonLinearizableHistoryT;

  SmartMinimizorT() = delete;

  explicit SmartMinimizorT(int exploration_runs, int minimization_runs,
                           PrettyPrinter& pretty_printer,
                           int max_offsprings_per_generation = 5,
                           int offsprings_generation_attemps = 10,
                           int initial_mutations_count = 10)
      : exploration_runs(exploration_runs),
        minimization_runs(minimization_runs),
        max_offsprings_per_generation(max_offsprings_per_generation),
        offsprings_generation_attemps(offsprings_generation_attemps),
        mutations_count(initial_mutations_count),
        pretty_printer(pretty_printer) {
    std::random_device dev;
    rng = std::mt19937(dev());
  }

  void Minimize(SchedulerT& sched,
                NonLinearizableHistoryT& nonlinear_histories) const override {
    // reset
    Strategy& strategy = sched.GetStrategy();
    total_tasks = strategy.GetTotalTasksCount();
    population.clear();
    population.insert(Solution(strategy, nonlinear_histories, total_tasks));

    for (int r = 0; r < minimization_runs; ++r) {
      const Solution& p1 = *population.begin();
      const Solution& p2 =
          *(population.size() < 2 ? population.begin()
                                  : std::next(population.begin()));

      std::vector<Solution> offsprings = GenerateOffsprings(sched, p1, p2);
      for (auto& s : offsprings) {
        population.insert(s);
      }

      while (population.size() > max_population_size) {
        population.erase(std::prev(population.end()));
      }
    }

    assert(!population.empty());
    const Solution& best_solution = *population.begin();

    RemoveInvalidTasks(strategy, best_solution.tasks);


    auto ordering = Base::GetTasksOrdering(best_solution.nonlinear_histories.full,
                                      std::unordered_set<int>{});

    ReplayMode mode =
    (best_solution.nonlinear_histories.reason ==
     NonLinearizableHistoryT::Reason::DEADLOCK)
        ? ReplayMode::NoForceComplete
        : ReplayMode::CompleteOnLast;

auto replayed = sched.ReplayRound(ordering, mode);

// Для deadlock replay теперь должен воспроизводить deadlock и возвращать has_value().
assert(replayed.has_value());
nonlinear_histories = replayed.value();
  }

 private:
  struct Solution {
    explicit Solution(const Strategy& strategy,
                      const NonLinearizableHistoryT& histories,
                      int total_tasks)
        : nonlinear_histories(histories) {
      const int total_threads = strategy.GetThreadsCount();

      // save valid task ids per thread
      const auto& threads = strategy.GetTasks();
      for (int i = 0; i < static_cast<int>(threads.size()); ++i) {
        for (int j = 0; j < static_cast<int>(threads[i].size()); ++j) {
          const auto& task = threads[i][j].get();
          if (!strategy.IsTaskRemoved(task->GetId())) {
            valid_tasks++;
            tasks[i].insert(task->GetId());
          }
        }
      }

      float tasks_fitness =
          1.0f - (valid_tasks * 1.0f) / (total_tasks * 1.0f);
      float threads_fitness =
          eps + 1.0f - (tasks.size() * 1.0f) / (total_threads * 1.0f);

      assert(tasks_fitness >= 0.0f && tasks_fitness <= 1.0f);
      assert(threads_fitness >= 0.0f && threads_fitness <= 1.0f);

      fitness = tasks_fitness * threads_fitness;
    }

    float GetFitness() const { return fitness; }
    int GetValidTasks() const { return valid_tasks; }

    std::unordered_map<int, std::unordered_set<int>> tasks;
    NonLinearizableHistoryT nonlinear_histories;

   private:
    float eps = 0.0001f;
    float fitness = 0.0f;
    int valid_tasks = 0;
  };

  struct SolutionSorter {
    bool operator()(const Solution& a, const Solution& b) const {
      return a.GetFitness() > b.GetFitness();
    }
  };

  void DropRandomTask(
      std::unordered_map<int, std::unordered_set<int>>& threads) const {
    if (threads.empty()) return;

    int thread_index =
        std::uniform_int_distribution<int>(0, static_cast<int>(threads.size()) - 1)(rng);
    auto it = std::next(threads.begin(), thread_index);
    auto& tasks = it->second;

    if (tasks.empty() ||
        (tasks.size() == 1 && threads.size() == 2)) {
      return;
    }

    int task_index =
        std::uniform_int_distribution<int>(0, static_cast<int>(tasks.size()) - 1)(rng);
    auto task_it = std::next(tasks.begin(), task_index);
    tasks.erase(task_it);
  }

  std::vector<Solution> GenerateOffsprings(SchedulerT& sched,
                                           const Solution& p1,
                                           const Solution& p2) const {
    assert(offsprings_generation_attemps > 0);
    Strategy& strategy = sched.GetStrategy();
    std::vector<Solution> offsprings;

    for (int offspring = 1; offspring <= max_offsprings_per_generation;
         ++offspring) {
      int left_attempts = offsprings_generation_attemps;
      while (left_attempts--) {
        auto new_threads = CrossProduct(strategy, &p1, &p2);

        for (int m = 1; m <= mutations_count; ++m) {
          if (mutations_count > 1 || dist(rng) < 0.95) {
            DropRandomTask(new_threads);
          }
        }

        RemoveInvalidTasks(strategy, new_threads);

        auto histories = sched.ExploreRound(exploration_runs);

        if (histories.has_value()) {
          offsprings.emplace_back(strategy, histories.value(), total_tasks);
          break;
        }
      }
    }

    if (static_cast<int>(offsprings.size()) * 2 < max_offsprings_per_generation &&
        mutations_count > 1) {
      mutations_count--;
    }

    return offsprings;
  }

  void RemoveInvalidTasks(
      Strategy& strategy,
      const std::unordered_map<int, std::unordered_set<int>>& valid_threads) const {
    const auto& tasks = strategy.GetTasks();
    for (int thread_id = 0; thread_id < static_cast<int>(tasks.size()); ++thread_id) {
      const auto& thread = tasks[thread_id];
      bool thread_exists = valid_threads.contains(thread_id);

      for (int i = 0; i < static_cast<int>(thread.size()); ++i) {
        int id = thread[i]->GetId();
        if (thread_exists && valid_threads.at(thread_id).contains(id)) {
          strategy.SetTaskRemoved(id, false);
        } else {
          strategy.SetTaskRemoved(id, true);
        }
      }
    }
  }

  std::unordered_map<int, std::unordered_set<int>> CrossProduct(
      const Strategy& /*strategy*/, const Solution* p1, const Solution* p2) const {
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
  const int max_offsprings_per_generation;
  const int offsprings_generation_attemps;

  const int max_population_size = 2;

  mutable int mutations_count;
  mutable int total_tasks{0};
  mutable std::multiset<Solution, SolutionSorter> population;
  mutable std::mt19937 rng;
  mutable std::uniform_real_distribution<double> dist{0.0, 1.0};

  PrettyPrinter& pretty_printer;
};

// Backward-compatible aliases:
using SmartMinimizor = SmartMinimizorT<HistoryEvent>;
using DualSmartMinimizor = SmartMinimizorT<DualHistoryEvent>;