## folly::channels

- **folly::channels::Channel<T> (пара Sender<T>/Receiver<T>)** - канал “отправитель→получатель”.
  **Total:** Sender::write(...), Sender::close(...), Receiver::cancel().
  **Partial:** co_await receiver.next() (получить следующее значение).
  **Условие ожидания:** нет готового значения и канал ещё не закрыт/не отменён (тогда next() ждёт, пока придёт значение или пока канал закроют/отменят).

---

## folly::coro (корутины)

- **folly::coro::Baton** - “батон” для корутин: можно co_await до post(). Не cancellation-aware как примитив.
  **Total:** post(), reset(), ready().
  **Partial:** co_await baton (через operator co_await).
  **Условие ожидания:** батон ещё не в signalled/posted состоянии (ready()==false).

- **folly::coro::Mutex** - async-mutex для корутин; lock берётся через co_await.
  **Total:** try_lock(), unlock().
  **Partial:** co_await m.co_lock() / co_await m.co_scoped_lock().
  **Условие ожидания:** mutex занят другим владельцем (тогда co_lock() suspend’ится).

- **folly::coro::SharedMutexFair (и using SharedMutex = SharedMutexFair)** - RW-lock (shared/exclusive/upgrade) для корутин, с fairness-очередностью и важными ограничениями использования.
  **Total:** try_lock(), try_lock_shared(), try_lock_upgrade(), unlock-операции (как “разрешители”).
  **Partial:** co_lock(), co_lock_shared(), co_lock_upgrade() и scoped-варианты.
  **Условие ожидания:** конфликт по режимам (writer ждёт ухода readers; readers могут ждать writer’а/очередь), плюс fairness-логика (в комментариях есть предупреждение про возможный deadlock при повторном read-lock).

- **folly::coro::BoundedQueue<T>** - bounded очередь: и enqueue, и dequeue awaitable (внутри два семафора “место/элементы”).
  **Total:** try_enqueue(...), try_dequeue(...), empty(), size().
  **Partial:** co_await enqueue(...) (ждёт место), co_await dequeue() (ждёт элемент), co_try_dequeue_for(timeout) (ждёт до таймаута/отмены).
  **Условие ожидания:** enqueue ждёт, когда есть свободная ёмкость; dequeue ждёт, когда есть элемент.

- **folly::coro::UnboundedQueue<T>** - unbounded очередь с async-ожиданием на dequeue() (через семафор).
  **Total:** enqueue(...) (всегда добавляет и сигналит), плюс обычно try_dequeue()/прочие неблокирующие.
  **Partial:** co_await dequeue() / co_try_dequeue_for(timeout).
  **Условие ожидания:** очередь пуста (тогда dequeue() ждёт, пока кто-то сделает enqueue() или пока операция не будет отменена/прервана таймаутом).

- **folly::coro::SmallUnboundedQueue<T>** - компактная очередь; поддерживает только enqueue(T) и dequeue(), dequeue() ждёт через coro::Baton, cancellation-aware.
  **Total:** enqueue(...).
  **Partial:** co_await dequeue().
  **Условие ожидания:** локальный буфер пуст (тогда регистрируется waiter и ожидание идёт до прихода сообщений или отмены).

- **folly::coro::AsyncPipe<T>** - “AsyncGenerator + write end”: publisher делает write(), consumer читает через generator.next(); есть close().
  **Total:** write(...) -> bool, close() / close(exception), isClosed().
  **Partial:** чтение на стороне AsyncGenerator (по смыслу next() ждёт, пока pipe что-то запишут или закроют).
  **Условие ожидания:** нет готового элемента и pipe ещё не закрыт.

- **folly::coro::BoundedAsyncPipe<T>** - bounded-вариант AsyncPipe: write() становится корутиной и может ждать capacity; есть try_write() без ожидания.
  **Total:** try_write(...), close() / close(exception), isClosed().
  **Partial:** co_await write(...) (может блокироваться на семафоре capacity).
  **Условие ожидания:** нет доступной ёмкости (tokens/capacity), пока reader не “дренирует” элементы.

- **folly::coro::Promise<T> / folly::coro::Future<T>** - контракт “promise выставляет результат, future co_await-ится”.
  **Total (resolver):** Promise::setValue(...), setException(...), setResult(...) (фулфилят и ready.post()).
  **Partial:** co_await Future (через operator co_await/WaitOperation).
  **Условие ожидания:** promise ещё не fulfilled (тогда future ждёт на внутреннем coro::Baton ready).

- **folly::coro::SharedPromise<T>** - “один promise → много awaitable futures” (обёртка вокруг coro Promise/Future, аналог folly::SharedPromise).
  **Total (resolver):** setValue(...), setException(...), setTry(...).
  **Partial:** futures, полученные из getFuture(), могут co_await-иться до fulfil.
  **Условие ожидания:** значение/исключение ещё не выставлено.

---

## folly::fibers (fibers)

- **folly::fibers::Baton** - усыпить текущий fiber и разбудить из другого fiber/потока (post). Есть timed-версии.
  **Total:** post(), ready(), reset(), try_wait() (не блокирует).
  **Partial:** wait(), try_wait_for(...), try_wait_until(...) (могут усыпить fiber до post/timeout).
  **Условие ожидания:** baton не posted (или ждём до deadline).

- **folly::fibers::Semaphore** - fiber-compatible семафор (FIFO wakeup).
  **Total:** signal(), try_wait().
  **Partial:** wait() (усыпляет fiber при отсутствии токенов), coro::Task co_wait() и co_try_wait_for(timeout) (для корутин, cancellation-aware).
  **Условие ожидания:** нет доступных токенов/ёмкости.

---

## folly::synchronization (потоки)

- **folly::Baton (из folly/synchronization/Baton.h)** - “однократная передача сигнала”: поток может один раз блокироваться на wait() до post() (с оговорками жизненного цикла).
  **Total:** post(), try_wait().
  **Partial:** wait(), try_wait_for(...), try_wait_until(...).
  **Условие ожидания:** baton ещё не posted (или ждём до deadline).

- **folly::Latch** - как std::latch, но с timed waits: ждём, пока счётчик станет 0.
  **Total (resolver):** count_down(n), arrive_and_wait(n) (делает count_down и ждёт).
  **Partial:** wait() (внутри ждёт на семафоре).
  **Условие ожидания:** счётчик ещё не достиг нуля.

- **folly::SaturatingSemaphore** - “флаг/семафор 0→READY”, много waiters, много posters; post() идемпотентен после READY.
  **Total:** post(), reset(), ready(), try_wait().
  **Partial:** wait(), try_wait_until(...), try_wait_for(...) (могут блокировать).
  **Условие ожидания:** флаг ещё NOTREADY (не posted).

- **folly::LifoSem** - семафор, оптимизированный под производительность (LIFO wakeups), поддерживает shutdown, который будит/прерывает waiters.
  **Total (resolver):** post(n), shutdown(), isShutdown().
  **Partial:** wait() (и timed/tryWait варианты в полном API).
  **Условие ожидания:** нет доступных “единиц семафора” (и не в shutdown-состоянии).

- **folly::ThrottledLifoSem** - LifoSem с throttling wakeups (будит не чаще чем раз в wakeUpInterval), полезно для task queues.
  **Total (resolver):** post(...) (по смыслу семафора).
  **Partial:** wait()/try_wait_until/... (внутри ждёт/спит).
  **Условие ожидания:** нет доступных единиц + throttling может задерживать пробуждение.

- **folly::EventCount** - “condvar для lock-free”: waiter делает prepareWait(), проверяет predicate, потом wait(key) или cancelWait(), poster делает notify/notifyAll().
  **Total (resolver):** notify(), notifyAll(), cancelWait().
  **Partial:** wait(Key).
  **Условие ожидания:** пользовательское условие/predicate ещё false, и waiter дошёл до wait(key) (с допуском spurious wakeups, predicate надо перепроверять).

- **folly::atomic_wait / atomic_notify_one / atomic_notify_all (AtomicNotification)** - “как futex”: можно ждать на atomic и будить notify.
  **Total (resolver):** atomic_notify_one/all.
  **Partial:** atomic_wait(...) и timed-варианты.
  **Условие ожидания:** atomic остаётся в ожидаемом состоянии (значение не изменилось) - тогда atomic_wait блокирует до notify/изменения.

---

## folly::executors::task_queue (блокирующие очереди задач)

- **folly::BlockingQueue<T> (интерфейс)** - абстракция “добавить задачу / взять задачу”; некоторые реализации могут блокировать на full/empty.
  **Total/Partial:** зависит от реализации; типично take() может блокировать, add() обычно не блокирует (или блокирует только в bounded/BLOCK режимах).

- **folly::UnboundedBlockingQueue<T, Semaphore, UMPMCQueue>** - unbounded blocking queue: take() ждёт на семафоре, add() постит семафор.
  **Total:** add(T&&).
  **Partial:** take(), try_take_for(timeout) (ждут элемент/до таймаута).
  **Условие ожидания:** очередь пуста (нет задач) - тогда take() блокируется до add().

- **folly::LifoSemMPMCQueue<T, QueueBehaviorIfFull, Semaphore>** - bounded MPMC task queue: в режиме THROW add() бросает при full; в режиме BLOCK add() может блокироваться (через blockingWrite). take() ждёт на семафоре.
  **Total:** add() в режиме THROW (не ждёт, но может бросить), try_take_for(timeout) (ограниченное ожидание).
  **Partial:** take() (ждёт задачу); add() в режиме BLOCK (ждёт место в очереди).
  **Условие ожидания:** take() ждёт когда нет задач; add(BLOCK) ждёт когда очередь заполнена.