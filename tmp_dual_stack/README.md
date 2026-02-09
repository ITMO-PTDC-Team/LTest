## Что уже сделано 

### Идея (как dual поддерживаются)
- Dual‑операция (awaitable) рассматривается как 2 фазы: `Request` и `FollowUp`.
- Dual‑история содержит события:  
  `RequestInvoke → RequestResponse → FollowUpInvoke → FollowUpResponse`.
- Awaitable исполняется внутри LTest Task (fiber), но “resume” устроен так, чтобы структура данных не продолжала другую LTest‑операцию напрямую:
  - в `await_suspend(h)` передаётся handle waker‑корутины,
  - структура вызывает `h.resume()` при матчинге,
  - waker делает `UnblockAllOn(addr)`,
  - scheduler потом сам возобновляет заблокированную LTest‑таску.

---

# 1) Dual execution: как исполняются dual операции

### Что сделано
- Добавлен `target_method_dual` и `TargetDualMethod` wrapper, который:
  - вручную вызывает `await_ready / await_suspend / await_resume`,
  - при ожидании делает `SetBlocked + CoroYield` в цикле,
  - пишет dual-события в буфер задачи (`EmitDualEvent`),
  - маркирует задачу как dual (`SetDual(true)`).

### Файлы
- `runtime/include/verifying_macro.h` — `TargetDualMethod`, `target_method_dual`.

---

# 2) Dual events: буферизация событий внутри задачи

### Что сделано
- В `CoroBase` добавлено:
  - флаг dual-задачи `SetDual/IsDual`,
  - очередь dual-событий `pending_dual_events_`,
  - методы `EmitDualEvent` / `DrainDualEvents`.

### Файлы
- `runtime/include/lib.h`, `runtime/lib.cpp`.

---

# 3) Dual scheduler: сбор dual истории и запуск dual checker

### Что сделано
- Добавлен отдельный `DualStrategyScheduler`, который:
  - на старте dual-task пишет `RequestInvoke`,
  - после каждого `Resume()` сливает `DrainDualEvents()` в историю,
  - для обычных операций продолжает писать `Invoke/Response`,
  - при успехе/ошибке печатает dual историю.

### Файлы
- `runtime/include/scheduler.h` — `DualStrategyScheduler`.

---

# 4) Dual checker: проверка расширенной истории

### Что сделано
- Введены типы dual событий (`RequestInvoke/...`) и `DualHistoryEvent`.
- Реализован `LinearizabilityDualCheckerRecursive`, который линеаризует request/followup по dual‑спеке.
- Dual‑спека задаётся через `GetDualMethods()` (nonblocking или request+followup).

### Файлы
- `runtime/include/lincheck_dual.h`.

---

# 5) Dual runner: запуск dual режима

### Что сделано
- Добавлен `SpecDual`, `RunDual`, `LTEST_ENTRYPOINT_DUAL`.
- Dual режим использует `LinearizabilityDualCheckerRecursive` + `DualStrategyScheduler`.

### Файлы
- `runtime/include/verifying.h`.

---

# Что пока НЕ сделано 
- Dual replay
- minimization: нет.
- Dual TLA: не поддержан.
- 2ой смысл deadlock
- делегировать резолвинг дедлок пользователю
- ABA

Кажется идейно умею для
- stack/queue/channel
- bound/unbound
- c одной/двух сторон
- mutex/RW-lock


Сейчас:

все история
текущие состояние
тред 
->
vector<task> 

канал go
libcoro
folly




`GetWorkloadPolicy()` возвращает правила типа:

- `wait_method` — операция, которая может “занять поток ожиданием” (например `"pop"`)
- `progress_methods` — операции, которые могут продвинуть систему (например `{"push"}`)
- `reserve_threads` — сколько потоков надо “оставить” под прогресс (например `1`)

100 потоков

100 пушей 
99 pop

stack:
cnt_pop <= cnt_push + cnt_thread - 1

channel:
cnt_pop <= cnt_push + cnt_thread - 1
cnt_push <= cnt_pop + cnt_thread - 1

mutex:
per thread: lock() - unlock()

queue/mutex/exchanger
hash table with condition 



Пример для стека:
- если все pop → последний свободный поток должен выбирать push


```c++
auto x = co_await something;
```
-->
```c++
auto a = something; // awaitable
if (!a.await_ready()) {
  std::coroutine_handle<> h = /* current coroutine handle */;

  if (a.await_suspend(h)) {
    return; // управление уйдёт наружу
  }
}
// await_ready==true или await_suspend==false
auto x = a.await_resume();
```


https://timsong-cpp.github.io/cppwp/n4861/expr.await
