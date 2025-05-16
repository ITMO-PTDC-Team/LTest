#include "minimization_smart.h"

#include "scheduler.h"

// smart minimizor
SmartMinimizor::SmartMinimizor(int exploration_runs, int minimization_runs,
                               PrettyPrinter& pretty_printer,
                               int max_offsprings_per_generation,
                               int offsprings_generation_attemps,
                               int initial_mutations_count)
    : exploration_runs(exploration_runs),
      minimization_runs(minimization_runs),
      pretty_printer(pretty_printer),
      max_offsprings_per_generation(max_offsprings_per_generation),
      offsprings_generation_attemps(offsprings_generation_attemps),
      mutations_count(initial_mutations_count) {
  std::random_device dev;
  rng = std::mt19937(dev());
}

void SmartMinimizor::Minimize(
    SchedulerWithReplay& sched,
    Scheduler::NonLinearizableHistory& nonlinear_histories) const {
  // reset
  auto& strategy = sched.GetStrategy();
  total_tasks = strategy.GetTotalTasksCount();
  population.clear();
  population.insert(Solution(strategy, nonlinear_histories, total_tasks));

  // TODO: when the mutations count is 1, we could add a counter which will
  // increase,
  //       when it reaches the `max_nonsuccessful_runs` then we make a fast exit
  //       (cannot minimize anymore)
  for (int r = 0; r < minimization_runs; ++r) {
    // TODO: select with probability (allow to select worse parents as well)
    const Solution& p1 = *population.begin();
    const Solution& p2 =
        *(population.size() < 2 ? population.begin()
                                : std::next(population.begin()));

    std::vector<Solution> offsprings =
        GenerateOffsprings(sched, p1, p2);  // includes mutations
    for (auto& s : offsprings) {
      population.insert(s);
    }

    while (population.size() > max_population_size) {
      population.erase(std::prev(population.end()));
    }
  }

  // final answer
  assert(!population.empty());  // at least the 1st solution should be there
  const Solution& best_solution = *population.begin();

  // put tasks in a valid state according to the found best solution
  RemoveInvalidTasks(strategy, best_solution.tasks);

  // replay the round with found nonlinearized interleaving, this put the round
  // in correct final state and builds a `BothHistories` object
  auto replayed_result = sched.ReplayRound(RoundMinimizor::GetTasksOrdering(
      best_solution.nonlinear_histories.full, {}));

  // override nonlinear history with the best solution
  assert(replayed_result.has_value());
  nonlinear_histories = replayed_result.value();
}

void SmartMinimizor::DropRandomTask(
    std::unordered_map<int, std::unordered_set<int>>& threads) const {
  if (threads.empty()) return;

  // pick a thread from which to drop task
  int thread_index =
      std::uniform_int_distribution<int>(0, threads.size() - 1)(rng);
  auto it = std::next(threads.begin(), thread_index);
  auto& tasks = it->second;

  if (tasks.empty() ||
      tasks.size() == 1 &&
          threads.size() == 2  // removing task from selected thread will result
                               // in single thread left
  )
    return;

  // remove task with position `task_index` from picked thread
  int task_index = std::uniform_int_distribution<int>(0, tasks.size() - 1)(rng);
  auto task_it = std::next(tasks.begin(), task_index);
  tasks.erase(task_it);
}

std::vector<SmartMinimizor::Solution> SmartMinimizor::GenerateOffsprings(
    SchedulerWithReplay& sched, const Solution& p1, const Solution& p2) const {
  assert(offsprings_generation_attemps > 0);
  Strategy& strategy = sched.GetStrategy();
  std::vector<Solution> offsprings;

  for (int offspring = 1; offspring <= max_offsprings_per_generation;
       ++offspring) {
    int left_attempts = offsprings_generation_attemps;
    while (left_attempts--) {
      // cross product
      auto new_threads = CrossProduct(strategy, &p1, &p2);

      // mutations
      int applied_mutations = 0;
      for (int m = 1; m <= mutations_count; ++m) {
        // If we permit only a single mutation, then with some probability
        // no mutations will be applyed at all. This is done to sometimes
        // generate offspring that mixes the parent threads without changing the
        // tasks count.
        if (mutations_count > 1 || dist(rng) < 0.95) {
          applied_mutations++;
          DropRandomTask(new_threads);
        }
      }

      // check for nonlinearizability
      // 1. mark only valid tasks in round as non-removed
      RemoveInvalidTasks(strategy, new_threads);

      // 2. explore round in order to find non-linearizable history
      auto histories = sched.ExploreRound(exploration_runs);

      // 3. new offspring successfully generated
      if (histories.has_value()) {
        Solution offspring(strategy, histories.value(), total_tasks);
        offsprings.push_back(offspring);
        break;
      }
    }
  }

  // This is an optimization which decreases the number of permitted mutations
  // over time when many unsuccessfull attempts to generate offspring are made.
  if (offsprings.size() * 2 < max_offsprings_per_generation &&
      mutations_count > 1) {
    // update the mutations count
    mutations_count--;
  }

  return offsprings;
}

void SmartMinimizor::RemoveInvalidTasks(
    Strategy& strategy,
    const std::unordered_map<int, std::unordered_set<int>>& valid_threads)
    const {
  const auto& tasks = strategy.GetTasks();
  for (int thread_id = 0; thread_id < tasks.size(); ++thread_id) {
    const auto& thread = tasks[thread_id];
    bool thread_exists = valid_threads.contains(thread_id);

    for (int i = 0; i < thread.size(); ++i) {
      if (thread_exists &&
          valid_threads.at(thread_id).contains(thread[i]->GetId())) {
        strategy.SetTaskRemoved(thread[i]->GetId(), false);
      } else {
        strategy.SetTaskRemoved(thread[i]->GetId(), true);
      }
    }
  }
}

std::unordered_map<int, std::unordered_set<int>> SmartMinimizor::CrossProduct(
    const Strategy& strategy, const Solution* p1, const Solution* p2) const {
  // p1 has smaller number of threads
  if (p1->tasks.size() >= p2->tasks.size()) {
    std::swap(p1, p2);
  }

  // probability of copying thread tasks from p1
  const float p = 0.5;
  std::unordered_map<int, std::unordered_set<int>> new_threads;

  // TODO:
  //  1. `p` could be weighted according to the number of tasks in thread
  //     (the smaller the tasks count, the bigger probability of selecting this
  //     thread)
  //  2. if threads count differs, then we could compare the tasks count with
  //     another thread index (e.g. if p1: [T1, T2] and p2: [T1, T2, T3, T4],
  //     then compare p1:T2 not only with p2:T2, but with p2:T3 and p2:T4 as
  //     well)

  for (auto& [thread_id, task_ids] : p1->tasks) {
    if (p2->tasks.contains(thread_id) && dist(rng) >= p) {
      new_threads[thread_id] = p2->tasks.at(thread_id);
    } else {
      new_threads[thread_id] = task_ids;
    }
  }

  return new_threads;
}

// smart minimizor solution
SmartMinimizor::Solution::Solution(
    const Strategy& strategy,
    const Scheduler::NonLinearizableHistory& histories, int total_tasks) {
  int total_threads = strategy.GetThreadsCount();

  // copy nonlinear history
  nonlinear_histories = histories;

  // save valid task ids per thread
  const auto& threads = strategy.GetTasks();
  for (int i = 0; i < threads.size(); ++i) {
    for (int j = 0; j < threads[i].size(); ++j) {
      const auto& task = threads[i][j].get();

      if (!strategy.IsTaskRemoved(task->GetId())) {
        valid_tasks++;
        tasks[i].insert(task->GetId());
      }
    }
  }

  // cache the fitness value of the solution
  float tasks_fitness =
      1.0 -
      (valid_tasks * 1.0) /
          (total_tasks *
           1.0);  // the less tasks left, the closer tasks fitness is to 1.0
  float threads_fitness =
      eps + 1.0 -
      (tasks.size() * 1.0) /
          (total_threads *
           1.0);  // the less threads left, the closer threads fitness is to 1.0

  assert(tasks_fitness >= 0.0 && tasks_fitness <= 1.0);
  assert(threads_fitness >= 0.0 && threads_fitness <= 1.0);

  fitness =
      tasks_fitness * threads_fitness;  // in [0.0, 1.0]: the bigger, the better
}

float SmartMinimizor::Solution::GetFitness() const { return fitness; }

int SmartMinimizor::Solution::GetValidTasks() const { return valid_tasks; }