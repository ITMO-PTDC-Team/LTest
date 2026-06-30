# Проверка дуальных структур в LTest

Документ описывает текущую модель проверки дуальных операций в LTest:
как строится история, как работает checker, как писать спецификации и как
запускать дуальные targets.

## Что такое дуальная операция

Обычный LTest target проверяется как история пар:

```text
Invoke(method(args)) -> Response(result)
```

Для awaitable/coroutine API этого недостаточно. Операция может:

1. зарегистрировать ожидание или сразу опубликовать эффект;
2. заблокироваться до встречного события;
3. позже возобновиться и вернуть результат.

В дуальном режиме такая операция раскладывается на request/follow-up:

```text
RequestInvoke(method(args))
RequestResponse(request_done)
FollowUpInvoke
FollowUpResponse(result)
```

Смысл фаз:

- `RequestInvoke` - пользователь вызвал дуальный метод.
- `RequestResponse` - request-фаза завершилась: awaiter зарегистрирован,
  операция либо уже готова, либо ожидает внешнего прогресса.
- `FollowUpInvoke` - операция возобновилась и начинает получать результат.
- `FollowUpResponse` - `await_resume()` завершился и вернул результат.

Обычные методы могут жить в той же истории. Они по-прежнему дают
`Invoke/Response`. Поэтому дуальная история имеет тип `DualHistoryEvent` и
может смешивать `Invoke`, `Response`, `RequestInvoke`, `RequestResponse`,
`FollowUpInvoke`, `FollowUpResponse`.

Основные файлы:

- `runtime/include/verifying_macro.h` - `target_method_dual` и awaiter-wrapper.
- `runtime/include/lincheck_dual.h` - типы дуальной истории и checker.
- `runtime/include/scheduler.h` - `DualStrategyScheduler`, `DualTLAScheduler`,
  replay, minimization, deadlock handling.
- `runtime/include/verifying.h` - `SpecDual`, `RunDual`,
  `LTEST_ENTRYPOINT_DUAL`.

## Как runtime строит историю

Дуальная цель объявляется через `target_method_dual`:

```cpp
target_method_dual(genArgs, Ret, Target, method, Args...);
```

Макрос регистрирует builder задачи и помечает созданную задачу как dual.
`DualStrategyScheduler` пишет `RequestInvoke` при первом старте такой задачи,
после каждого `Resume()` забирает накопленные task-local dual events и добавляет
их в историю в порядке глобального `seqno`.

События внутри задачи генерируются wrapper-ом из `TargetDualMethod`:

1. вызывает target method и получает awaiter/awaitable;
2. вызывает `await_ready()`;
3. если awaiter готов, сразу пишет `RequestResponse`, `FollowUpInvoke`,
   вызывает `await_resume()` и пишет `FollowUpResponse`;
4. если awaiter не готов, вызывает `await_suspend()`, пишет
   `RequestResponse`, блокирует LTest-задачу через `BlockManager` и ждёт
   waker;
5. после wakeup пишет `FollowUpInvoke`, вызывает `await_resume()` и пишет
   `FollowUpResponse`.

Wrapper также управляет временем жизни awaitable-объектов через `KeepAlive`,
отложенным уничтожением coroutine handles через `DeferDestroy`, и умеет
вызывать `unregister()` у awaiter-а при принудительном завершении раунда.

Для некоторых внешних awaiter-ов есть два opt-in флага:

- `ltest_emit_request_before_suspend = true` - request считается завершённым
  до вызова `await_suspend()`. Это нужно, если сам `await_suspend()` ожидает,
  что request уже виден checker-у.
- `ltest_cleanup_before_target_destroy = true` - deferred cleanup выполняется
  до уничтожения target-объекта. Это нужно для awaiter-ов, которые при cleanup
  обращаются к объекту примитива.

## Контракт спецификации

Дуальный target использует `SpecDual` и entrypoint:

```cpp
using spec_t = ltest::SpecDual<Target, spec::MySpec, MyHash, MyEquals>;
LTEST_ENTRYPOINT_DUAL(spec_t);
```

`RunDual()` создаёт `LinearizabilityDualCheckerRecursive` из
`spec::MySpec::GetDualMethods()` и начального состояния `spec::MySpec{}`.

Спецификация должна вернуть `DualMethodMap<SpecState>`:

```cpp
static auto GetDualMethods() {
  using S = MySpec;
  DualMethodMap<S> m;

  m.emplace("try_op", DualNonBlockingMethod<S>{
      [](S* s, void* args) -> ValueWrapper {
        return s->TryOp(args);
      }});

  DualRequestMethod<S> wait_req =
      [](S* s, void* args, int op_id) {
        s->RequestWait(args, op_id);
      };

  DualFollowUpMethod<S> wait_fol =
      [](S* s, void* args, int op_id) -> std::optional<ValueWrapper> {
        return s->FollowUpWait(args, op_id);
      };

  m.emplace("wait", DualBlockingMethod<S>{wait_req, wait_fol});
  return m;
}
```

Правила:

- имя в `GetDualMethods()` должно совпадать с именем target method;
- обычный метод описывается `DualNonBlockingMethod` и возвращает
  `ValueWrapper`;
- дуальный метод описывается парой `request` и `followup`;
- `request` мутирует последовательную модель и получает `op_id`;
- `followup` возвращает `std::nullopt`, если операция ещё не может
  завершиться в этом состоянии модели;
- `followup` возвращает `ValueWrapper`, когда результат готов;
- состояние спецификации должно быть copy-constructible;
- для cache эффективны корректные hash/equals, особенно на больших историях.

Примеры:

- `verifying/specs/coro_async_stack.h` - `push` обычный, `pop` дуальный.
- `verifying/specs/coroutine_queue.h` - `send` и `receive` оба дуальные.
- `verifying/specs/libcoro/mutex.h` - `lock` дуальный,
  `try_lock/unlock` обычные.
- `verifying/specs/folly/coro_bounded_queue.h` - смешанная спецификация для
  bounded queue.

## Как работает checker

`LinearizabilityDualCheckerRecursive` адаптирует дуальную историю к общему
рекурсивному поиску линеаризации из `linearization_search.h`.

Для каждой candidate linearization checker пробует применить ещё не
линеаризованный invoke-событие, если до первого незакрытого response оно
остаётся допустимым:

- `Invoke` обычного метода применяет `DualNonBlockingMethod` и сравнивает
  результат с `Response`;
- `RequestInvoke` дуального метода применяет `request`;
- `FollowUpInvoke` дуального метода применяет `followup`; если `followup`
  вернул `std::nullopt`, такой порядок невозможен;
- если response есть, checker линеаризует invoke и соответствующий response
  вместе;
- pending invoke без response может быть отброшен как незавершённая операция.

Особый случай для дуальных операций: если `RequestResponse` уже есть, но
`FollowUpResponse` ещё нет, request считается видимой pending-операцией. В конце
каждой найденной линеаризации checker дополнительно вызывает `followup` для
таких pending request-ов на копии состояния. Если `followup` уже готов вернуть
значение, история отклоняется: реализация должна была иметь возможность
завершить операцию, но в observed history этого не произошло.

Именно это позволяет отличать корректное ожидание от ошибки вида
`request` сделал операцию готовой, но coroutine так и не была возобновлена.

## WorkloadPolicy и deadlock

Для дуальных структур deadlock часто означает не ошибку реализации, а
неинформативную нагрузку. Например, если все потоки начали `pop()` на пустой
очереди и никто не может стартовать `push()`, дальнейшего прогресса нет.

Спецификация может вернуть `GetWorkloadPolicy()`:

```cpp
static ltest::WorkloadPolicy GetWorkloadPolicy() {
  ltest::WorkloadPolicy p;

  ltest::ReserveRule r;
  r.wait_method = "pop";
  r.progress_methods = {"push"};
  r.reserve_threads = 1;
  p.reserve.push_back(std::move(r));

  ltest::PrefixBudgetRule pb;
  pb.wait_method = "pop";
  pb.progress_methods = {"push"};
  pb.reserve_threads = 1;
  pb.initial_credit = 0;
  p.prefix_budget.push_back(std::move(pb));

  return p;
}
```

Поддерживаемые правила:

- `reserve` - если уже есть blocked `wait_method`, сохранить потоки для
  `progress_methods`;
- `prefix_budget` - ограничить число стартов wait-операций относительно
  стартов progress-операций и начального ресурса;
- `rollback` - подсказка для `--deadlock_policy=rollback`: какую
  progress-операцию добавить вместо blocked wait-операции;
- `max_active` - ограничить число одновременно активных задач метода;
- `max_blocked` - ограничить число одновременно заблокированных задач метода.

Флаг запуска:

```text
--deadlock_policy=fail|checker|explore|rollback
```

Семантика:

- `fail` - видимый deadlock считается ошибкой, процесс возвращает код `4`;
- `checker` - при deadlock проверить partial history; если она
  линеаризуема, раунд принимается как неинформативный;
- `explore` - если partial history линеаризуема, scheduler пробует соседние
  interleavings через `ExploreRound`;
- `rollback` - если partial history линеаризуема, scheduler пытается удалить
  blocked wait-задачу, добавить progress-метод из policy и replay-нуть раунд;
  затем fallback к `explore`.

Для acceptance-прогонов дуальных структур обычно используется:

```text
--deadlock_policy=explore
```

`fail` полезен как диагностический режим, но для корректных дуальных структур
он может падать на валидной, но неинформативной нагрузке.

## Коды возврата

`TrapRunDual()` возвращает:

- `0` - `success!`, нарушений не найдено;
- `3` - `non linearized:`, checker нашёл нелинеаризуемую историю;
- `4` - `deadlock detected:`, deadlock был признан ошибкой политики.

При `--minimize=true` scheduler сначала печатает полный сценарий, потом
пытается сократить его тем же interleaving, rescheduling и smart minimizer-ом.

## Сборка

Базовая конфигурация:

```bash
cmake -G Ninja -B build -DCMAKE_BUILD_TYPE=Release
```

Собрать все доступные дуальные цели:

```bash
cmake --build build --target verify-dual
```

`verify-dual` зависит от:

- локальных целей из `verifying/dual`;
- `verify-dual-libcoro`;
- `verify-dual-folly`;
- `verify-dual-vk`.

Группы `libcoro`, `folly` и `vk` собираются только если CMake нашёл их
зависимости. Если зависимостей нет, CMake печатает `Skipping ... dual targets`
и создаёт пустую custom target-заглушку.

Условия по группам:

- `libcoro`: нужен `coro/task.hpp` и либо установленная библиотека
  `coro/libcoro`, либо checkout в `LTEST_LIBCORO_SOURCE_DIR`
  (по умолчанию `/tmp/libcoro`).
- `folly`: нужны headers/lib Folly, `glog`, `libevent`,
  `double-conversion`, `fmt`.
- `vk`: нужны `verifying/dual/eng`, `verifying/dual/vk`,
  `function2/function2.hpp`, `fmt`.

Пример для libcoro checkout:

```bash
cmake -G Ninja -B build-libcoro \
  -DCMAKE_BUILD_TYPE=Release \
  -DLTEST_LIBCORO_SOURCE_DIR=/tmp/libcoro
cmake --build build-libcoro --target verify-dual-libcoro
```

## Запуск отдельной цели

Core example:

```bash
cmake --build build --target coro_async_stack
./build/verifying/dual/coro_async_stack \
  --strategy pct \
  --threads 3 \
  --tasks 8 \
  --rounds 1000 \
  --deadlock_policy=explore
```

libcoro example:

```bash
cmake --build build-libcoro --target libcoro_queue
./build-libcoro/verifying/dual/libcoro/libcoro_queue \
  --strategy pct \
  --threads 2 \
  --tasks 4 \
  --rounds 30 \
  --deadlock_policy=explore
```

Folly example:

```bash
cmake --build build-folly --target folly_coro_mutex
./build-folly/verifying/dual/folly/folly_coro_mutex \
  --strategy random \
  --threads 2 \
  --tasks 4 \
  --rounds 30 \
  --seed 311 \
  --deadlock_policy=explore
```

Если dynamic linker не находит runtime или gflags, выставить `LD_LIBRARY_PATH`
как в matrix-скриптах:

```bash
export LD_LIBRARY_PATH="$PWD/build/runtime:$PWD/build/codegen:$PWD/build/third_party/gflags/lib:$PWD/build/runtime/third_party/gflags/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
```

Для другого build-dir заменить `build` на нужный каталог.

## Matrix-скрипты

Скрипты запускают несколько стратегий и параметров для группы targets, пишут
логи в `/tmp` и поддерживают переменные окружения.

libcoro:

```bash
BUILD_DIR="$PWD/build-libcoro" \
LOG_DIR=/tmp/ltest_libcoro_matrix/manual \
DEADLOCK_POLICY=explore \
./scripts/run_dual_libcoro_matrix.sh
```

Folly:

```bash
BUILD_DIR="$PWD/build-folly" \
LOG_DIR=/tmp/ltest_folly_matrix/manual \
DEADLOCK_POLICY=explore \
./scripts/run_dual_folly_matrix.sh
```

VK:

```bash
BUILD_DIR="$PWD/build-vk" \
LOG_DIR=/tmp/ltest_vk_matrix/manual \
DEADLOCK_POLICY=explore \
./scripts/run_dual_vk_matrix.sh
```

Общие полезные переменные:

- `BUILD_DIR` - где искать собранные бинарники;
- `LOG_DIR` - куда писать логи;
- `TIMEOUT` - timeout одного кейса, например `180s`;
- `RUN_TLA=0` - отключить TLA-варианты;
- `RUN_MINIMIZE=0` - отключить варианты с `--minimize=true`;
- `DEADLOCK_POLICY=explore|rollback|checker|fail`;
- `CONTINUE_ON_FAIL=1` - не останавливать матрицу на первом падении.

Для `libcoro` и `folly` матрицы дополнительно принимают:

- `ALLOW_KNOWN_FAILURES`;
- `INCLUDE_KNOWN_FAILURES` для libcoro.

## Основные флаги verifier-а

```text
--strategy=rr|random|pct|tla
--threads=N
--tasks=N
--rounds=N
--switches=N
--weights=3,1,2
--seed=N
--deadlock_policy=fail|checker|explore|rollback
--minimize=true
--exploration_runs=N
--minimization_runs=N
--verbose=true
```

Рекомендации:

- для быстрых smoke-прогонов использовать `random` или `pct`, небольшие
  `tasks/rounds` и фиксированный `--seed`;
- для acceptance дуальных целей использовать `--deadlock_policy=explore`;
- для поиска короткого воспроизведения добавить `--minimize=true`;
- для исчерпывающего маленького пространства использовать `--strategy=tla`,
  маленькие `--tasks` и `--switches`.

## Как добавить новый дуальный target

1. Написать wrapper target-объекта в `verifying/dual/...`.
2. Зарегистрировать awaitable методы через `target_method_dual`.
3. Зарегистрировать обычные методы через `target_method` или
   `target_method_async`.
4. Написать sequential spec в `verifying/specs/...`.
5. В spec реализовать `GetDualMethods()`.
6. Если возможны легальные ожидания, добавить `GetWorkloadPolicy()`.
7. Если API требует protocol constraints, добавить verifier и использовать
   `LTEST_ENTRYPOINT_DUAL_CONSTRAINT`.
8. Добавить `.cpp` в соответствующий `CMakeLists.txt`, если группа не
   использует glob.
9. Собрать target и сначала прогнать маленький deterministic запуск с `--seed`.
10. Затем прогнать matrix-скрипт или несколько стратегий вручную.

Минимальный target-скелет:

```cpp
#include "../specs/my_spec.h"
#include "../../runtime/include/verifying.h"

struct MyTarget {
  non_atomic auto wait_op() {
    return primitive.wait_op();
  }

  non_atomic void progress_op() {
    primitive.progress_op();
  }

  MyPrimitive primitive;
};

target_method_dual(ltest::generators::genEmpty, void, MyTarget, wait_op);
target_method(ltest::generators::genEmpty, void, MyTarget, progress_op);

using spec_t = ltest::SpecDual<MyTarget,
                               spec::MySpec,
                               spec::MySpecHash,
                               spec::MySpecEquals>;

LTEST_ENTRYPOINT_DUAL(spec_t);
```

## Где смотреть примеры

- `verifying/dual/coro_async_stack.cpp` и
  `verifying/specs/coro_async_stack.h` - самый простой смешанный пример.
- `verifying/dual/coroutine_queue.cpp` и
  `verifying/specs/coroutine_queue.h` - две встречные дуальные операции.
- `verifying/dual/libcoro/libcoro_queue.cpp` и
  `verifying/specs/libcoro/queue.h` - внешний coroutine task API.
- `verifying/dual/folly/folly_coro_await_adapter.h` - adapters для Folly
  awaiter/task/future lifetime и cancellation.
- `verifying/blocking/verifiers/libcoro_mutex_verifier.h` и
  `verifying/dual/folly/verifiers/folly_coro_mutex_verifier.h` - protocol
  constraints и cleanup release tasks.
