// RUN: %check
#include <cstdlib>

void g(void*);

void test(std::size_t size) {
  // CHECK: %[[name:[0-9]+]] = call ptr @LtestMemAlloc(i64 100)
  // CHECK-NEXT: store ptr %[[name]], ptr %[[loc:[0-9]+]]
  void* f = malloc(100);
  g(f);
  // CHECK: call
  // CHECK-NEXT: %[[res:[0-9]+]] = load ptr, ptr %[[loc]]
  // CHECK-NEXT: call void @LtestMemDealloc(ptr %[[res]])
  free(f);
}
