## TODO LIST

### Codegen

* ~~Добавить генерацию основных блоков корутины~~
* ~~Добавить кастомный промис~~
* ~~Добавить генерацию кода для вызова других корутин~~
* ~~Добавить генерацию suspend операций после каждой инструкции IR~~
* ~~Добавить регресионные тесты~~
* ~~Учитывать mangling~~
* Научиться работать с разными типами данных

### Scheduler
* ~~StackfullTask~~
* ~~basic classes~~
* round robin class
* ~~tests~~
* ~~lincheck research~~
* ~~basic lincheck~~
* ~~поработать над кодом в целом~~
* подумать что делать с параметрами методов
* учитывать количество тредов в планировщике
* research статьи про то как лучше строить исполнения
* isBusy для примтивов синхронизации
* threads - количество потоков влияет на порядок

### General
* Разобраться со временем жизни корутин и задач, точно ли
```c++
using Task = std::shared_ptr<CoroBase>;
```
* ~~Избавиться от легаси verifying/verify.py и сделать нормальный cli~~
* Очистка памяти, когда мы вынужденно завершаем корутину, сейчас реализована как просто попытка дождаться нормального завершения корутины, что не совсем верно (иногда задача ожидает изменения из другой задачи и при неправильном порядке мы можем прождать вечно). На текущий момент закостылено через сброс состояния структуры Reset(), с предположением, что реализация Reset() даст завершение всем ожидающим задачам. Это как-то нужно переделать.
* NOTE(svileex): наверное по хорошему нужен граф ожиданий, он всегда ациклический, поэтому можно будет сделать топсорт и позавершать

### Blocking-related
* Блокирование корутин по фьютексу реализовано несколько странно, нужно подумать над улучшением.
