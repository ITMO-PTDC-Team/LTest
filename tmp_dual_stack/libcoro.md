2 смысла DEADLOCK
->
user делать constraint на историю

- необходимое и достаточное условие


## Нотация

Есть N акторов (потоки/корутины).
Для каждого метода op и каждого момента времени t считаем:

- Req(op, t) - число акторов, которые сейчас выполняют request‑фазу.
- Blk(op, t) - число акторов, которые закончили request‑фазу, но не могут начать followup, т.е. ждут resume.
- Fol(op, t) - число акторов, которые сейчас выполняют followup‑фазу op.
- Cmp(op, t) - число акторов, которые полностью завершили op.

Из них:

- Inv(op, t) = Req(op,t) + Blk(op,t) + Fol(op,t) + Cmp(op,t) - сколько раз op уже было вызван к моменту t.

---

# libcoro: список структур

## 1) coro::event

Коротко: ручное событие. Можно co_await, пока его не установят. Если уже set, ожидание не блокирует.

- Partial: co_await event
- Total: set(), reset(), is_set()
- Условие ожидания: event не set

//Inv(set, t) = 0 -> 

(Req(co_await, t) + Blk(co_await, t) + Fol(co_await, t) <= N - 1)

Проблема с reset()...
reset() : set -> not set
T0                                  T1
..... 
set() = true

reset() <- invoke     
                                    co_await event <- invoke 
                                    if (set() == true)
                                      победа
                      
реально сделал set из true в false

reset() <- response

co_await event <- invoke


Req(co_await, t) = 2
N = 2
2 <= 1 <- false 

- reply() (уже есть)
- посмотреть у Lincheck
- consumer/produce что надо

---

## 2) coro::latch

Коротко: счётчик до нуля. Один/несколько акторов ждут, пока другие сделают нужное число count_down().

- Partial: co_await latch (wait)
- Total: count_down(n), remaining(), is_ready()
- Условие ожидания: remaining() > 0.

Down(t) = Σ (n_j) по всем операциям count_down в inv

``c++
Down(t) < K ->  (Inv(wait,t) <= N − 1)
``

---

## 3) coro::mutex

Коротко: mutex для корутин. Захват может co_await-иться, при конкуренции корутина засыпает.

- Partial: lock()
- Total: try_lock(), unlock()
- Условие ожидания: mutex уже захвачен другим актором.

---

## 4) coro::shared_mutex

Коротко: RW‑lock (shared/exclusive). Могут ждать и readers, и writer’ы; есть политика очередности (anti-starvation: если
есть waiting writer, новые readers ждут за ним).

- Partial: lock_shared(), lock()
- Total: try_lock_shared(), try_lock()
- Условие ожидания:
    - для lock() (exclusive): есть активные readers или active writer,
    - для lock_shared() (shared): active writer или fairness-очередь (writer ждёт -> новые readers встают за ним).

---

## 5) coro::semaphore<max_value>

Коротко: семафор ресурсов (с максимальным значением). acquire() может ждать ресурс. Есть shutdown(), после которого
ожидатели просыпаются и acquire начинает завершаться “как shutdown”.

- Partial: acquire()
- Total: try_acquire(), release(), shutdown(), is_shutdown(), value(), max()
- Условие ожидания: семафор не shutdown и ресурсов сейчас нет (условно value()==0).

---

## 6) coro::queue<T>

Коротко: async MPMC очередь (unbounded). pop() ждёт, пока появится элемент, либо пока очередь не остановят

- Partial: pop()
- Total: push(value), try_pop(), empty(), size(), shutdown(), shutdown_drain(executor), is_shutdown()
- Условие ожидания: очередь пуста и состояние не stopped (running/draining).

Inv(Shutdown, t) + Inv(ShutdownDrain, t) == 0 -> (Inv(Pop, t) ≤ Q0 + Cmp(Push,t) + (N − 1))
                                                                        ^---учитывать только успешные

---

## 7) coro::ring_buffer<T, num_elements>

Коротко: bounded MPMC ring buffer. produce() ждёт место (если full), consume() ждёт элемент (если empty).

- Partial: produce(element) (может ждать место), consume() (может ждать элемент)
- Total: try_produce(...), try_consume(), empty(), full(), size(), capacity(), shutdown(), shutdown_drain(executor),
  notify_producers(), notify_consumers(), is_shutdown()
- Условие ожидания:
    - produce ждёт: full() и состояние running,
    - consume ждёт: empty() и состояние не stopped (до shutdown).

Пусть:
- `InvStop(t) = Inv(Shutdown,t) + Inv(ShutdownDrain,t)`
- `CmpProducedPush(t)` = число завершённых `push/emplace`, которые вернули `queue_produce_result::produced` (а не `stopped`)
- `CmpProdOK(t)` = число завершённых `produce()`, вернувших `ring_buffer_result::produce::produced` 
- `CmpConsOK(t)` = число завершённых `consume()`, которые вернули элемент

### 1: потребители не могут все уйти в ожидание на пустом
InvStop(t)+Inv(notify_consumers,t)=0 --> (Inv(Consume,t) <= B0 + CmpProdOK(t) + (N-1))
### Формула 2: производители не могут все уйти в ожидание на полном
InvStop(t)+Inv(notify_producers,t)=0 --> (Inv(Produce,t) <= (Cap-B0) + CmpConsOK(t) + (N-1))


---

## 8) coro::condition_variable

Коротко: условная переменная для корутин. wait может усыплять до notify_* и/или выполнения пользовательского predicate,
также могут быть stop_token/timeout варианты.

- Partial: wait(...), wait(lock, predicate), wait(lock, stop_token, predicate), timed wait_for/ wait_until
- Total (resumers): notify_one(), notify_all()
- Условие ожидания: предикат “ещё не true” и/или не было notify; плюс stop/timeout (если включены) могут прекращать
  ожидание.

тут про условие
  