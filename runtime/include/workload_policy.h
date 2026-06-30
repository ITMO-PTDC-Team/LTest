#pragma once

#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

namespace ltest {

// Если уже есть заблокированные операции wait_method, то при нехватке
// свободных потоков разрешаем стартовать только progress_methods.
struct ReserveRule {
  std::string wait_method;
  std::vector<std::string> progress_methods;
  std::size_t reserve_threads = 1;
};

// Префиксное ограничение на число стартов wait-операций.
//
// Формула:
//
//   Inv(wait_method)
//     <= initial_credit
//      + sum(Inv(progress_methods))
//      + threads
//      - reserve_threads
//
// Где:
// - initial_credit — начальный запас ресурса/прогресса
// - progress_methods — методы, которые могут продвинуть wait_method
struct PrefixBudgetRule {
  std::string wait_method;
  std::vector<std::string> progress_methods;
  std::size_t reserve_threads = 1;
  std::size_t initial_credit = 0;
};

// Directed deadlock recovery for search.
//
// If deadlock_policy=rollback sees a blocked wait_method and the partial
// history is still linearizable, the scheduler may remove one such wait
// operation from the generated round, append one of progress_methods, and
// replay the modified round.
struct RollbackRule {
  std::string wait_method;
  std::vector<std::string> progress_methods;
};

// Ограничение на число одновременно активных незавершённых задач данного метода.
struct MaxActiveRule {
  std::string method;
  std::size_t max_active = 0;
};

// Ограничение на число одновременно заблокированных задач данного метода.
struct MaxBlockedRule {
  std::string method;
  std::size_t max_blocked = 0;
};

// Policy, которую может вернуть spec.
struct WorkloadPolicy {
  std::vector<ReserveRule> reserve;
  std::vector<PrefixBudgetRule> prefix_budget;
  std::vector<RollbackRule> rollback;
  std::vector<MaxActiveRule> max_active;
  std::vector<MaxBlockedRule> max_blocked;
};

// Контекст, который scheduler передаёт verifier/policy при старте новой задачи.
struct StartContext {
  std::size_t threads = 0;

  // Сколько потоков сейчас свободны для старта новой задачи.
  std::size_t free_threads = 0;

  // Число активных незавершённых задач по имени метода.
  std::unordered_map<std::string, int> active_by_method;

  // Число заблокированных задач по имени метода.
  std::unordered_map<std::string, int> blocked_by_method;
};

}  // namespace ltest
