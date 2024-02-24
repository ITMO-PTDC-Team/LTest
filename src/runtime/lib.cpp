#include "include/lib.h"

#include <cassert>
#include <iostream>
#include <vector>

extern "C" {

// This structure must be equal to the clone in LLVM pass.
struct CoroPromise {
  using handle = std::coroutine_handle<CoroPromise>;

  int has_ret_val{};
  int ret_val{};
  handle child_hdl{};
  char *name{};
};

CoroPromise *get_promise(std::coroutine_handle<CoroPromise> hdl) {
  return &hdl.promise();
}

// We must keep promise in the stack (LLVM now doesn't support heap promises).
// So there is no `new_promise()` function,
// instead we allocate promise at the stack in codegen pass.
void init_promise(CoroPromise *p, char *name) {
  assert(p != nullptr);

  p->child_hdl = nullptr;
  p->ret_val = 0;
  p->has_ret_val = 0;
  p->name = name;
}

handle get_child_hdl(CoroPromise *p) {
  assert(p != nullptr);
  assert(p->child_hdl && "p has not child");

  return p->child_hdl;
}

bool has_child_hdl(CoroPromise *p) {
  assert(p != nullptr);

  return p->child_hdl != nullptr;
}

void set_child_hdl(CoroPromise *p, int8_t *hdl) {
  assert(p != nullptr);

  p->child_hdl = std::coroutine_handle<CoroPromise>::from_address(hdl);
}

void set_ret_val(CoroPromise *p, int ret_val) {
  assert(p != nullptr);

  p->has_ret_val = 1;
  p->ret_val = ret_val;
}

bool has_ret_val(CoroPromise *p) {
  assert(p != nullptr);

  return p->has_ret_val;
}

int get_ret_val(CoroPromise *p) {
  assert(p != nullptr);
  assert(p->has_ret_val && "promise has not ret val");

  return p->ret_val;
}

Task make_task(handle hdl) { return Task(hdl); }

TaskBuilderList new_task_builder_list() {
  return new std::vector<TaskBuilder>();
}

void destroy_task_builder_list(TaskBuilderList l) { delete l; }

void push_task_builder_list(TaskBuilderList l, TaskBuilder builder) {
  l->push_back(builder);
}

char *get_name(CoroPromise *p) {
  assert(p != nullptr);
  assert(p->name != nullptr);
  return p->name;
}
}

Task::Task(handle hdl) : hdl(hdl) {}

void Task::Resume() {
  assert(!IsReturned() && "returned task can not be resumed");
  hdl.resume();
}

bool Task::HasChild() { return has_child_hdl(&hdl.promise()); }

Task Task::GetChild() {
  assert(HasChild() && "get_child() can not be called on childless task");
  return make_task(hdl.promise().child_hdl);
}

void Task::ClearChild() { set_child_hdl(&hdl.promise(), nullptr); }

bool Task::IsReturned() { return has_ret_val(&hdl.promise()); }

int Task::GetRetVal() { return get_ret_val(&hdl.promise()); }

std::string Task::GetName() const {
  return std::string{get_name(&hdl.promise())};
}

StackfulTask::StackfulTask(Task task) : entrypoint(task) {
  stack = std::vector<Task>{task};
}

void StackfulTask::Resume() {
  assert(!stack.empty());
  Task &stack_head = stack.back();
  stack_head.Resume();

  if (stack_head.HasChild()) {
    // new child was forked
    stack.push_back(stack_head.GetChild());
  } else if (stack_head.IsReturned()) {
    // stack_head returned
    last_returned_value = stack_head.GetRetVal();

    // if it wasn't the first task clean up children
    if (stack.size() >= 2) {
      auto previous = stack[stack.size() - 2];
      previous.ClearChild();
    }
  }
}

bool StackfulTask::IsReturned() { return stack.empty(); }

int StackfulTask::GetRetVal() const { return last_returned_value; }
const std::string &StackfulTask::GetName() const {
  return entrypoint.value().GetName();
}

const StackfulTask &StackfulTaskInvoke::GetTask() const {
  return this->task.get();
}

const StackfulTask &StackfulTaskResponse::GetTask() const {
  return this->task.get();
}
