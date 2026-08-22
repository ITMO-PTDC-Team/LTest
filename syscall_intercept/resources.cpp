#include <dlfcn.h>

#include "runtime/include/coro_ctx_guard.h"
#include "runtime/include/mock_res.h"
using namespace ltest;

static void *(*real_calloc)(size_t nmemb, size_t size);
static void *(*real_malloc)(size_t size);
static void (*real_free)(void *ptr);
static void *(*real_realloc)(void *ptr, size_t size);

void *malloc(size_t size) {
  // //write(2, "malloc\n", 7);
  if (!real_malloc) {
    reinterpret_cast<void *&>(real_malloc) = dlsym(RTLD_NEXT, "malloc");
  }
  void *p = real_malloc(size);
  if (ltest_coro_ctx) {
    ltest::SchedCtxGuard guard;
    memory_handler->RememberPointer(p);
  }
  return p;
}

namespace ltest {
bool inside_free = false;
}  // namespace ltest

void free(void *ptr) {
  if (inside_free) {
    return;
  }
  // write(2, "free\n", 5);
  inside_free = true;
  if (!real_free) {
    reinterpret_cast<void *&>(real_free) = dlsym(RTLD_NEXT, "free");
  }
  // A pointer can be allocated in a coroutine and freed later from scheduler
  // destructor code after ltest_coro_ctx has been cleared.
  if (memory_handler && ptr != nullptr) {
    ltest::SchedCtxGuard guard;
    memory_handler->ForgetAboutPointer(ptr);
  }
  real_free(ptr);
  inside_free = false;
}

void *calloc(size_t nmemb, size_t size) {
  // write(2, "calloc\n", 7);
  if (!real_calloc) {
    reinterpret_cast<void *&>(real_calloc) = dlsym(RTLD_NEXT, "calloc");
  }
  void *p = real_calloc(nmemb, size);
  if (ltest_coro_ctx) {
    ltest::SchedCtxGuard guard;
    memory_handler->RememberPointer(p);
  }
  return p;
}

void *realloc(void *ptr, size_t size) {
  // write(2, "realloc\n", 8);
  if (!real_realloc) {
    reinterpret_cast<void *&>(real_realloc) = dlsym(RTLD_NEXT, "realloc");
  }
  void *p = real_realloc(ptr, size);
  if (ltest_coro_ctx && p != ptr) {
    ltest::SchedCtxGuard guard;
    memory_handler->ForgetAboutPointer(ptr);
    if (p != nullptr) {
      memory_handler->RememberPointer(p);
    }
  }
  return p;
}
