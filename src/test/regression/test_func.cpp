#include <cassert>
#include <iostream>
#include <optional>
#include <vector>

#include "../../runtime/include/lib.h"

extern "C" {

int var{};
void tick() { ++var; }

// This function runs `test` task until it and all children are terminated.
void run(TaskBuilderList l) {
  std::optional<Task> task;
  for (auto task_builder : *l) {
    auto cur_task = task_builder();
    if (cur_task.GetName() == "test") {
      task = cur_task;
      break;
    }
  }
  assert(task.has_value() && "task `test` is not found");

  // Keep stack that contains launched tasks.
  std::vector<Task> stack = {task.value()};
  while (stack.size()) {
    auto current = stack.back();
    if (current.IsReturned()) {
      std::cout << "returned " << current.GetRetVal() << std::endl;
      stack.pop_back();
      if (!stack.empty()) {
        stack.back().ClearChild();
      }
    } else {
      current.Resume();
      std::cout << var << std::endl;
      if (current.HasChild()) {
        stack.push_back(current.GetChild());
      }
    }
  }
}
}