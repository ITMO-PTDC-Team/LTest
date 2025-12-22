//
// Created by d84370027 on 12/22/2025.
//
#pragma once

#include <future>
#include <memory>
#include <thread>

#include "dual_treiber_stack.cpp"

struct DualTreiberStackAsync {
public:
  //   std::future<int> f = stack.PopAsync();
  //   int x = co_await f;  // или f.get();
  std::future<int> PopAsync() {
    Ticket ticket = core.PopRequest();
  auto prom = std::make_shared<std::promise<int>>();
    std::future<int> fut = prom->get_future();

    std::thread([this, ticket, prom]() mutable {
      while (true) {
        int v = core.TryPopFollowUp(ticket);
        if (v != 0) {
          prom->set_value(v);
          break;
        }
      }
    }).detach();

    return fut;
  }

  void Push(int v) {
    core.Push(v);
  }
private:
  using Ticket = DualTreiberStack::Ticket;

  DualTreiberStack core;
};