## План

- не запускать корутину с не выполненым условием
сделал 
- при запуске обнулять условие
сделал
- при засыпанием проверить условие
- на паузе. Надо пробросить
- определять какой тип засыпания:
а) наш илд -> ничего особенного
б) его илд -> значит следующий будет followup
- WIP (прикрепить scheduler_dual_state.h в планировщик)

Вопрос:
а) дедлок? - есть. 
-просто счетчик
-последнию операцию 

б)

await_ready/await_suspend

pipeline 



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

LTest: 

1. вопрос валидация
strategy - дает функцию и поток (same)
checker - same

точек переключения = старые 
+ новые (специальную обработку)
+ scheduler меняю

Invoke(request), Response(Request), 
Invoke(FollowUp), Response(FollowUp, false),
...
Invoke(FollowUp), Response(FollowUp, false),
Invoke(FollowUp), Response(FollowUp, true)

2. вопрос нельзя дальше запускать



3. вопрос про promise?




Разберись в LTEST СНАЧАЛА: # LTEST
библиотека для проверки структур данных на линеризуемость (корректность в многопточном мире).

Немного о архитектуре:

- codegen раставляет в методах, где нужно точки переключения (используются корутины)
- strategy - выбирает следующую операцию, где будет исполняться до переключения
  (ищет не линеризуемое исполнение)
- checket - проверяет на существование эквивалентного последовательного исполнения сохранящее отнонение happens-before.
  (проверяет линеризуемо ли исполнение)
- minimizer - пытается уменьшить исполнение, чтобы оно оставалось некоректным.

Задача: я сейчас разбираюсь с проектом. Буду кидать файлы, а ты должен мне будешь объяснять что написано. Первое что кидаю, структуру проекта. Попытайся объяснить для чего основные файлы(tree -L 3): .
├── CMakeLists.txt
├── Dockerfile
├── README.md
├── TODO.md
├── build
│   ├── CMakeCache.txt
│   ├── CMakeFiles
│   │   ├── 3.25.1
│   │   ├── CMakeError.log
│   │   ├── CMakeOutput.log
│   │   ├── TargetDirectories.txt
│   │   ├── cmake.check_cache
│   │   ├── pkgRedirects
│   │   └── rules.ninja
│   ├── CPackConfig.cmake
│   ├── CPackSourceConfig.cmake
│   ├── CTestTestfile.cmake
│   ├── DartConfiguration.tcl
│   ├── Testing
│   │   └── Temporary
│   ├── _deps
│   │   ├── abseil-cpp-build
│   │   ├── abseil-cpp-src
│   │   ├── abseil-cpp-subbuild
│   │   ├── antlr_cpp-build
│   │   ├── antlr_cpp-src
│   │   ├── antlr_cpp-subbuild
│   │   ├── fuzztest-build
│   │   ├── fuzztest-src
│   │   ├── fuzztest-subbuild
│   │   ├── googletest-build
│   │   ├── googletest-src
│   │   ├── googletest-subbuild
│   │   ├── re2-build
│   │   ├── re2-src
│   │   └── re2-subbuild
│   ├── bin
│   ├── build.ninja
│   ├── clangpass
│   │   ├── CMakeFiles
│   │   ├── CTestTestfile.cmake
│   │   ├── ClangPassTool
│   │   └── cmake_install.cmake
│   ├── cmake_install.cmake
│   ├── codegen
│   │   ├── CMakeFiles
│   │   ├── CTestTestfile.cmake
│   │   ├── cmake_install.cmake
│   │   ├── libYieldPass.so -> libYieldPass.so.19.1
│   │   └── libYieldPass.so.19.1
│   ├── compile_commands.json
│   ├── lib
│   │   └── pkgconfig
│   ├── options-pinned.h
│   ├── runtime
│   │   ├── CMakeFiles
│   │   ├── CTestTestfile.cmake
│   │   ├── cmake_install.cmake
│   │   └── libruntime.so
│   ├── syscall_intercept
│   │   ├── CMakeFiles
│   │   ├── CTestTestfile.cmake
│   │   └── cmake_install.cmake
│   ├── test
│   │   ├── CMakeFiles
│   │   ├── CTestTestfile.cmake
│   │   ├── cmake_install.cmake
│   │   └── runtime
│   ├── third_party
│   │   ├── CMakeFiles
│   │   └── cmake_install.cmake
│   └── verifying
│       ├── CMakeFiles
│       ├── CTestTestfile.cmake
│       ├── blocking
│       ├── cmake_install.cmake
│       └── targets
├── clangpass
│   ├── CMakeLists.txt
│   ├── ast_consumer.cpp
│   ├── clangpass_tool.cpp
│   ├── include
│   │   └── clangpass.h
│   └── refactor_matcher.cpp
├── cmake-build-debug
│   ├── CMakeCache.txt
│   ├── CMakeFiles
│   │   ├── 3.28.3
│   │   ├── CMakeConfigureLog.yaml
│   │   ├── CMakeScratch
│   │   ├── clion-Debug-log.txt
│   │   ├── clion-environment.txt
│   │   ├── cmake.check_cache
│   │   └── pkgRedirects
│   ├── CPackConfig.cmake
│   ├── CPackSourceConfig.cmake
│   ├── DartConfiguration.tcl
│   ├── Testing
│   │   └── Temporary
│   ├── _deps
│   │   ├── abseil-cpp-build
│   │   ├── abseil-cpp-src
│   │   ├── abseil-cpp-subbuild
│   │   ├── antlr_cpp-build
│   │   ├── antlr_cpp-src
│   │   ├── antlr_cpp-subbuild
│   │   ├── fuzztest-build
│   │   ├── fuzztest-src
│   │   ├── fuzztest-subbuild
│   │   ├── googletest-build
│   │   ├── googletest-src
│   │   ├── googletest-subbuild
│   │   ├── re2-build
│   │   ├── re2-src
│   │   └── re2-subbuild
│   ├── clangpass
│   │   └── CMakeFiles
│   ├── options-pinned.h
│   └── third_party
│       └── CMakeFiles
├── codegen
│   ├── CMakeLists.txt
│   ├── coyieldpass.cpp
│   └── yieldpass.cpp
├── docker-compose.yml
├── example
│   └── siisti.cpp
├── runtime
│   ├── CMakeLists.txt
│   ├── coro_ctx_guard.cpp
│   ├── generators.cpp
│   ├── include
│   │   ├── block_manager.h
│   │   ├── block_state.h
│   │   ├── blocking_primitives.h
│   │   ├── coro_ctx_guard.h
│   │   ├── generators.h
│   │   ├── lib.h
│   │   ├── lincheck.h
│   │   ├── lincheck_recursive.h
│   │   ├── logger.h
│   │   ├── minimization.h
│   │   ├── minimization_smart.h
│   │   ├── pct_strategy.h
│   │   ├── pick_strategy.h
│   │   ├── pretty_print.h
│   │   ├── random_strategy.h
│   │   ├── round_robin_strategy.h
│   │   ├── scheduler.h
│   │   ├── scheduler_fwd.h
│   │   ├── stable_vector.h
│   │   ├── strategy_verifier.h
│   │   ├── value_wrapper.h
│   │   ├── verifying.h
│   │   └── verifying_macro.h
│   ├── lib.cpp
│   ├── lin_check.cpp
│   ├── logger.cpp
│   ├── minimization.cpp
│   ├── minimization_smart.cpp
│   ├── pretty_printer.cpp
│   └── verifying.cpp
├── scripts
│   ├── check.sh
│   ├── check_ctx_speed.sh
│   ├── format_code.sh
│   └── rund.sh
├── syscall_intercept
│   ├── CMakeLists.txt
│   └── hook.cpp
├── tempory
│   └── text
├── test
│   ├── CMakeLists.txt
│   └── runtime
│       ├── CMakeLists.txt
│       ├── lin_check_test.cpp
│       └── stackfulltask_mock.h
├── third_party
│   └── CMakeLists.txt
├── txt
└── verifying
├── CMakeLists.txt
├── README.md
├── blocking
│   ├── CMakeLists.txt
│   ├── bank.cpp
│   ├── bank_deadlock.cpp
│   ├── buffered_channel.cpp
│   ├── folly_flatcombining_queue.cpp
│   ├── folly_rwspinlock.cpp
│   ├── folly_sharedmutex.cpp
│   ├── mutexed_register.cpp
│   ├── nonlinear_buffered_channel.cpp
│   ├── nonlinear_mutex.cpp
│   ├── shared_mutexed_register.cpp
│   ├── simple_deadlock.cpp
│   ├── simple_mutex.cpp
│   └── verifiers
├── specs
│   ├── bank.h
│   ├── buffered_channel.h
│   ├── mutex.h
│   ├── queue.h
│   ├── register.h
│   ├── set.h
│   ├── stack.h
│   └── unique_args.h
└── targets
├── CMakeLists.txt
├── atomic_register.cpp
├── counique_args.cpp
├── counique_args.yml
├── fast_queue.cpp
├── nonlinear_ms_queue.cpp
├── nonlinear_queue.cpp
├── nonlinear_set.cpp
├── nonlinear_treiber_stack.cpp
├── race_register.cpp
└── unique_args.cpp

. Какие файлы скинуть первые?
