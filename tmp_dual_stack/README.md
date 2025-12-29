## План

# поддержка в целом корутин

LTest уже имеет механизм для инструментирования C++20 корутин?
Вроде да - codegen/yieldpacc.cpp

Вставка вызовов для переключения вокруг:

* await_ready - проверка готовности

* await_suspend - приостановка корутины

* await_resume - возобновление корутины

* initial_suspend/final_suspend - точки жизни корутины

Сейчас вроде умеем: 
Было:
```c++
co_await mutex.lock();

```
Стало:

```c++
CoroutineStatusChange("mutex_lock_suspend", true);  
co_await mutex.lock();
CoroutineStatusChange("mutex_lock_suspend", false); 

```

CoroutineStatusChange в lib.cpp
```c++
extern "C" void CoroutineStatusChange(char* name, bool start) {
// assert(!coroutine_status.has_value());
coroutine_status.emplace(name, start);
CoroYield();
}
```

Чего не делает:

* Не регистрирует корутину в BlockManager

* Не связывает ожидание с конкретным условием

* Не управляет пробуждением

Нужно создать связь:

Когда C++20 корутина ждет мьютекса -> соответствующая LTest корутина тоже ждет

? Когда C++20 корутина пробуждается -> LTest корутина тоже пробуждается
(или просто дается возможность пробудиться)

Подзадача:

* проверить точно ли так сейчас делает LLVM IR?
* поддерживает разные типы корутин

# History 


# BlockManager  + CoroutineStatusChange

Надо научиться регаться на ожидание.
Заходим и засыпаем на co_await.

Пробуждение - новая точка линеризации будет.

# Scheduler  + Strategy

# strategy_verifier?

# LinearizabilityChecker
WGL - у каждой функции одна точка линеризации 

Из-за этого хочется разбить на две функции. Чтобы не трогать эту часть.

У нас в коде частичный метод (например lock) он как один. Так что если мы запустили  lock, то мы уже не сможем выйти из него без успеха. WGL не хочется менять. Надо просто от вызов разбить на Invoke(request), Response(Request), Invoke(FollowUp), Response(FollowUp, false)..., Invoke(FollowUp), Response(FollowUp, true) и тогда ничего в WGL не надо менять


Предложение что-то типо:

```c++
extern "C" void CoroutineStatusChange(char* name, bool start) {
  coroutine_status.emplace(name, start);
  
  if (start) {
    // Вход в co_await - завершаем текущую операцию
    if (/* это первое ожидание для этой корутины */) {
      this_coro->CompleteOperation(REQUEST_COMPLETE, generate_token());
    } else {
      this_coro->CompleteOperation(FOLLOWUP_FAILED, current_token());
    }
    
    // Регистрируем в BlockManager
    BlockState state = parse_state(name);
    block_manager.BlockOn(state, this_coro.get());
    
  } else {
    // Выход из co_await - начинаем новую операцию
    if (/* условие выполнено */) {
      scheduler->CreateFollowupOperation(this_coro, FOLLOWUP_SUCCESS);
    } else {
      scheduler->CreateFollowupOperation(this_coro, FOLLOWUP_RETRY);
    }
  }
  
  CoroYield();
}
```

Кажется будет достаточно модификации планировщикая
+ пробросить инфу на этот уровень как-то?


