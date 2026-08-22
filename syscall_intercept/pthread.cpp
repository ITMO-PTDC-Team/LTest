
#include <pthread.h>

#include <cassert>
#include <dlfcn.h>
#include <utility>

#include "runtime/include/blocking_primitives.h"
#include "runtime/include/coro_ctx_guard.h"
#include "runtime/include/os_simulator.h"
#include "runtime/include/pthread_key.h"
using namespace ltest;

[[nodiscard]] bool ShouldUseMock() {
  return ltest_initialized && ltest_coro_ctx;
}

template <class Function>
Function GetRealPthreadFunction(const char* name) {
  Function function = nullptr;
  reinterpret_cast<void*&>(function) = dlsym(RTLD_NEXT, name);
  assert(function);
  return function;
}

#define PTHREAD_MOCK_INIT_ROUTINE(name, ...)                              \
  if (!ShouldUseMock()) {                                                 \
    static decltype(&name) real_fn = nullptr;                             \
    if (!real_fn) {                                                       \
      reinterpret_cast<void*&>(real_fn) = dlsym(RTLD_NEXT, #name);        \
      assert(real_fn);                                                    \
    }                                                                     \
    return real_fn(__VA_ARGS__);                                          \
  }                                                                       \
  ltest::SchedCtxGuard guard;

extern int pthread_create(pthread_t *__restrict __newthread,
                          const pthread_attr_t *__restrict __attr,
                          void *(*__start_routine)(void *),
                          void *__restrict __arg) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_create, __newthread, __attr,
                            __start_routine, __arg)
  assert(!thread_info.has_value());
  std::string name = "NewThread";
  thread_info.emplace(CreatedThreadInfo(
      [__start_routine, __arg](void *) {
        CoroYield();
        return __start_routine(__arg);
      },
      name));
  current_max_thread_id++;
  *__newthread = current_max_thread_id;
  {
    ltest::CoroCtxGuard coro_guard;
    CoroYield();
  }
  return 0;
}

int pthread_join(pthread_t __th, void **__thread_return) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_join, __th, __thread_return)
  assert(!thread_info.has_value());
  thread_info.emplace(JoinThreadInfo(__th, __thread_return));
  {
    ltest::CoroCtxGuard coro_guard;
    CoroYield();
  }
  return 0;
}

extern void pthread_exit(void *__retval) __attribute__((__noreturn__)) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_exit, __retval)
  this_coro->TerminateWith(ValueWrapper(__retval, GetDefaultCompator<void *>(),
                                        &GetThreadResultToString));
  {
    ltest::CoroCtxGuard coro_guard;
    CoroYield();
  }
  throw std::runtime_error(
      "code after CoroYield() here should never be executed, exception is "
      "thrown only to match "
      "noreturn attribute");
}

extern "C" int __cxa_thread_atexit(void (*)(void*), void*, void*) noexcept {
  // LTest deliberately has no thread-local lifetime model.
  return 0;
}

extern "C" int __cxa_thread_atexit_impl(void (*)(void*), void*, void*)
    noexcept {
  // LTest deliberately has no thread-local lifetime model.
  return 0;
}

extern "C" int __cxa_atexit(void (*)(void*), void*, void*) noexcept {
  // LTest abandons target and runtime objects instead of running process-exit
  // destructors against state that crossed simulated rounds.
  return 0;
}

extern "C" int pthread_atfork(void (*)(), void (*)(), void (*)()) noexcept {
  // fork() is not part of the simulated OS model.
  return 0;
}

extern "C" int __register_atfork(void (*)(), void (*)(), void (*)(),
                                  void*) noexcept {
  // glibc's pthread_atfork() delegates to this symbol. Interpose both entry
  // points so no process-global handler survives across simulated rounds.
  return 0;
}

extern int pthread_key_create(pthread_key_t* key,
                              void (*destructor)(void*)) __THROW {
  if (ltest::ShouldUseMockPthreadKeys()) {
    return ltest::CreateMockPthreadKey(key);
  }
  static decltype(&pthread_key_create) real_fn =
      GetRealPthreadFunction<decltype(&pthread_key_create)>("pthread_key_create");
  const int result = real_fn(key, destructor);
  if (result == 0) {
    ltest::RegisterRealPthreadKey(*key);
  }
  return result;
}

extern int pthread_key_delete(pthread_key_t key) __THROW {
  if (ltest::IsMockPthreadKey(key)) {
    ltest::DeletePthreadKey(key);
    return 0;
  }
  const int result = GetRealPthreadFunction<decltype(&pthread_key_delete)>(
      "pthread_key_delete")(key);
  if (result == 0) {
    ltest::DeletePthreadKey(key);
  }
  return result;
}

extern void* pthread_getspecific(pthread_key_t key) __THROW {
  if (!ltest::ShouldUseMockPthreadKeys()) {
    return GetRealPthreadFunction<decltype(&pthread_getspecific)>(
        "pthread_getspecific")(key);
  }
  ltest::SchedCtxGuard guard;
  return ltest::GetPthreadKeyValue(key);
}

extern int pthread_setspecific(pthread_key_t key, const void* value) __THROW {
  if (!ltest::ShouldUseMockPthreadKeys()) {
    return GetRealPthreadFunction<decltype(&pthread_setspecific)>(
        "pthread_setspecific")(key, value);
  }
  ltest::SchedCtxGuard guard;
  return ltest::SetPthreadKeyValue(key, value);
}

template <class... Ts>
struct Overloads : Ts... {
  using Ts::operator()...;
};

// we need to call this mutex method in almost each mutex function, because
// mutex can be initialized by macros
static decltype(mutexes)::mapped_type &InsertMutex(
    pthread_mutex_t *__mutex, const pthread_mutexattr_t *__mutexattr = nullptr) {
  auto it = mutexes.find(__mutex);
  if (it == mutexes.end()) {
    int type = PTHREAD_MUTEX_NORMAL;
    if (__mutexattr != nullptr) {
      const int result = pthread_mutexattr_gettype(__mutexattr, &type);
      assert(result == 0);
    } else {
      type = __mutex->__data.__kind;
    }
    const bool is_recursive = type == PTHREAD_MUTEX_RECURSIVE_NP;
    if (is_recursive) {
      it = mutexes.emplace(__mutex, recursive_mutex()).first;
    } else {
      it = mutexes.emplace(__mutex, mutex()).first;
    }
  }
  return it->second;
}

static decltype(mutexes)::mapped_type &FindMutex(pthread_mutex_t *__mutex,
                                                 const std::string &message) {
  // std::cerr << "inserted" << "\n";
  auto it = mutexes.find(__mutex);
  if (it == mutexes.end()) {
    throw ltest::TestFailure(message);
  }
  return it->second;
}
extern int pthread_mutex_init(pthread_mutex_t *__mutex,
                              const pthread_mutexattr_t *__mutexattr) __THROW {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_mutex_init, __mutex, __mutexattr)
  InsertMutex(__mutex, __mutexattr);
  return 0;
}

/* Destroy a mutex.  */
extern int pthread_mutex_destroy(pthread_mutex_t *__mutex) __THROW {
  auto it = mutexes.find(__mutex);
  if (it != mutexes.end()) {
    mutexes.erase(it);
    return 0;
  }
  PTHREAD_MOCK_INIT_ROUTINE(pthread_mutex_destroy, __mutex)
  return 0;
}

/* Try locking a mutex.  */
extern int pthread_mutex_trylock(pthread_mutex_t *__mutex) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_mutex_trylock, __mutex)
  auto &it = InsertMutex(__mutex);
  return std::visit(Overloads{[](auto &a) {
                      bool res = a.try_lock();
                      if (res) {
                        return 0;
                      }
                      return EBUSY;
                    }},
                    it);
}

/* Lock a mutex.  */
extern int pthread_mutex_lock(pthread_mutex_t *__mutex) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_mutex_lock, __mutex)
  auto &it = InsertMutex(__mutex);
  return std::visit(Overloads{[](auto &a) {
                      a.lock();
                      return 0;
                    }},
                    it);
}

/* Unlock a mutex.  */
extern int pthread_mutex_unlock(pthread_mutex_t *__mutex) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_mutex_unlock, __mutex)
  auto &it = FindMutex(__mutex, "unlock called before lock");
  return std::visit(Overloads{[](auto &a) {
                      a.unlock();
                      return 0;
                    }},
                    it);
}

// we need to call this cond method in almost each cond function, because
// mutex can be initialized by macros
static decltype(cond_variables)::mapped_type &InsertCond(pthread_cond_t *cond) {
  // std::cerr << "inserted" << "\n";
  auto it = cond_variables.find(cond);
  if (it == cond_variables.end()) {
    it = cond_variables.emplace(cond, ltest::condition_variable()).first;
  }
  return it->second;
}

extern int pthread_cond_init(
    pthread_cond_t *__restrict __cond,
    const pthread_condattr_t *__restrict __cond_attr) __THROW {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_cond_init, __cond, __cond_attr)
  return 0;
}

/* Destroy condition variable COND.  */
extern int pthread_cond_destroy(pthread_cond_t *__cond) __THROW {
  auto it = cond_variables.find(__cond);
  if (it != cond_variables.end()) {
    cond_variables.erase(it);
    return 0;
  }
  PTHREAD_MOCK_INIT_ROUTINE(pthread_cond_destroy, __cond)
  return 0;
}

extern int pthread_cond_signal(pthread_cond_t *__cond) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_cond_signal, __cond)
  auto &c = InsertCond(__cond);
  c.notify_one();
  return 0;
}

extern int pthread_cond_broadcast(pthread_cond_t *__cond) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_cond_broadcast, __cond)
  auto &c = InsertCond(__cond);
  c.notify_all();
  return 0;
}

extern int pthread_cond_wait(pthread_cond_t *__restrict __cond,
                             pthread_mutex_t *__restrict __mutex) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_cond_wait, __cond, __mutex)
  auto &m = InsertMutex(__mutex);
  auto &c = InsertCond(__cond);
  std::visit([&c](auto& mutex) { c.wait(mutex); }, m);
  return 0;
}

static decltype(shared_mutexes)::mapped_type &InsertSharedLock(
    pthread_rwlock_t *lock) {
  auto it = shared_mutexes.find(lock);
  if (it == shared_mutexes.end()) {
    it = shared_mutexes.emplace(lock, ltest::shared_mutex()).first;
  }
  return it->second;
}

extern int pthread_rwlock_init(
    pthread_rwlock_t *__restrict __rwlock,
    const pthread_rwlockattr_t *__restrict __attr) __THROW {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_init, __rwlock, __attr)
  return 0;
}

/* Destroy read-write lock RWLOCK.  */
extern int pthread_rwlock_destroy(pthread_rwlock_t *__rwlock) __THROW {
  auto it = shared_mutexes.find(__rwlock);
  if (it != shared_mutexes.end()) {
    shared_mutexes.erase(it);
    return 0;
  }
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_destroy, __rwlock)
  return 0;
}

/* Acquire read lock for RWLOCK.  */
extern int pthread_rwlock_rdlock(pthread_rwlock_t *__rwlock) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_rdlock, __rwlock)
  auto &it = InsertSharedLock(__rwlock);
  return std::visit(Overloads{[](auto &a) {
                      a.lock_shared();
                      return 0;
                    }},
                    it);
}

/* Try to acquire read lock for RWLOCK.  */
extern int pthread_rwlock_tryrdlock(pthread_rwlock_t *__rwlock) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_tryrdlock, __rwlock)
  auto &it = InsertSharedLock(__rwlock);
  return std::visit(Overloads{[](auto &a) {
                      return a.try_lock_shared() ? 0 : EBUSY;
                    }},
                    it);
}

/* Acquire write lock for RWLOCK.  */
extern int pthread_rwlock_wrlock(pthread_rwlock_t *__rwlock) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_wrlock, __rwlock)
  auto &it = InsertSharedLock(__rwlock);
  return std::visit(Overloads{[](auto &a) {
                      a.lock();
                      return 0;
                    }},
                    it);
}

/* Try to acquire write lock for RWLOCK.  */
extern int pthread_rwlock_trywrlock(pthread_rwlock_t *__rwlock) {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_trywrlock, __rwlock)
  auto &it = InsertSharedLock(__rwlock);
  return std::visit(Overloads{[](auto &a) {
                      return a.try_lock() ? 0 : EBUSY;
                    }},
                    it);
}

/* Unlock RWLOCK.  */
extern int pthread_rwlock_unlock(pthread_rwlock_t *__rwlock) __THROWNL {
  PTHREAD_MOCK_INIT_ROUTINE(pthread_rwlock_unlock, __rwlock)
  auto &it = InsertSharedLock(__rwlock);
  return std::visit(Overloads{[](auto &a) {
                      a.unlock();
                      return 0;
                    }},
                    it);
}

// omit implementation
extern int pthread_getcpuclockid(pthread_t __thread_id,
                                 __clockid_t *__clock_id) __THROW {
  return 0;
}
