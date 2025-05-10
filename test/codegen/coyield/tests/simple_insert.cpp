// RUN: %check
int g() { return 1; }
void based_on_pos(){

}

void ignored() {}
int f() {
  ignored();
  // CHECK: call void @CoroutineStatusChange(ptr [[name:@[0-9]+]], i1 true)
  // CHECK-NEXT: %[[res:.*]] = call noundef i32 @_Z1gv()
  return g();
  // CHECK: call void @CoroutineStatusChange(ptr [[name]], i1 false)
  // CHECK-NEXT: ret i32 %[[res]]
}

int ignored_f(){
  return g();
}