// RUN: %check
#include <iostream>
//CHECK: define dso_local noundef i32 @test_main(i32 noundef %0, ptr noundef %1)
int main(int argc, char** argv)
{
  std::cout << "Hello world!\n";
}
// CHECK: define dso_local noundef i32 @main(i32 noundef %0, ptr noundef %1)
// CHECK-NEXT: %3 = call i32 @ltest_main(i32 %0, ptr %1)
// CHECK-NEXT: ret i32 %3
