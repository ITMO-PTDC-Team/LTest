#pragma once
#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lib.h"
#include "lincheck.h"
#include "logger.h"
#include "stable_vector.h"
using std::string;
using std::to_string;
struct CreateNewThreadHistoryInfo {
  size_t created_thread_id;
  std::string_view name;
};

using FullHistoryWithThreads =
    std::vector<std::pair<int, std::variant<std::reference_wrapper<Task>,
                                            CoroutineStatus, CreateNewThreadHistoryInfo, WaitThreadInfo>>>;

template <typename T>
void Dfs(const StableVector<T>& arr, std::vector<bool>& visited, size_t i,
         std::vector<size_t>& ans) {
  ans.push_back(i);
  visited[i] = true;
  for (auto& u : arr[i].children) {
    if (!visited[u]) {
      Dfs(arr, visited, u, ans);
    }
  }
}

template <typename T>
void TopSort(const StableVector<T>& arr, std::vector<size_t>& ans) {
  std::vector<bool> visited(arr.size(), false);
  for (int i = 0; i < arr.size(); i++) {
    if (!visited[i]) {
      Dfs(arr, visited, i, ans);
    }
  }
}

template <class... Ts>
struct Overloads : Ts... {
  using Ts::operator()...;
};

struct PrettyPrinter {
  PrettyPrinter(size_t threads_num);

  /*
      Prints like this:
      *------------------*-------------------*
      |        T0        |        T1         |
      *------------------*-------------------*
      | [id] Push(2)     |                   |
      | <--- 0           |                   |
      |                  |  [id] Pop()       |
      |                  |  <--- 5           |
      *------------------*-------------------*
       <---------------->
           cell_width
  */
  template <typename Out_t>
  void PrettyPrint(const std::vector<std::variant<Invoke, Response>>& result,
                   Out_t& out) {
    auto get_thread_num = [](const std::variant<Invoke, Response>& v) {
      // Crutch.
      if (v.index() == 0) {
        return get<0>(v).thread_id;
      }
      return get<1>(v).thread_id;
    };

    int cell_width = 20;  // Up it if necessary. Enough for now.

    auto print_separator = [&out, this, cell_width]() {
      out << "*";
      for (int i = 0; i < threads_num; ++i) {
        for (int j = 0; j < cell_width; ++j) {
          out << "-";
        }
        out << "*";
      }
      out << "\n";
    };

    auto print_spaces = [&out](int count) {
      for (int i = 0; i < count; ++i) {
        out << " ";
      }
    };

    print_separator();
    // Header.
    out << "|";
    for (int i = 0; i < threads_num; ++i) {
      int rest = cell_width - 1 /*T*/ - to_string(i).size();
      print_spaces(rest / 2);
      out << "T" << i;
      print_spaces(rest - rest / 2);
      out << "|";
    }
    out << "\n";

    print_separator();

    auto print_empty_cell = [&]() {
      print_spaces(cell_width);
      out << "|";
    };

    // Rows.
    for (const auto& i : result) {
      int num = get_thread_num(i);
      out << "|";
      for (int j = 0; j < num; ++j) {
        print_empty_cell();
      }

      FitPrinter fp{out, cell_width};
      fp.Out(" ");
      if (i.index() == 0) {
        auto inv = get<0>(i);
        auto& task = inv.GetTask();
        fp.Out("[" + std::to_string(task->GetId()) + "] ");
        fp.Out(std::string{task->GetName()});
        fp.Out("(");
        const auto& args = task->GetStrArgs();
        for (int i = 0; i < args.size(); ++i) {
          if (i > 0) {
            fp.Out(", ");
          }
          fp.Out(args[i]);
        }
        fp.Out(")");
      } else {
        auto resp = get<1>(i);
        fp.Out("<-- " + to_string(resp.GetTask()->GetRetVal()));
      }
      assert(fp.rest > 0 && "increase cell_width in pretty printer");
      print_spaces(fp.rest);
      out << "|";

      for (int j = 0; j < threads_num - num - 1; ++j) {
        print_empty_cell();
      }
      out << "\n";
    }

    print_separator();
  }

  // Helps to debug full histories.
  template <typename Out_t>
  void PrettyPrint(FullHistoryWithThreads& result, std::vector<size_t> mapping,
                   Out_t& out) {
    int cell_width = 10;  // Up it if necessary. Enough for now.

    std::vector<int> inverse_mapping(mapping.size(), -1);
    for (int i = 0; i < mapping.size(); i++) {
      inverse_mapping[mapping[i]] = i;
    }
    auto print_separator = [&out, this, cell_width]() {
      out << "*";
      for (int i = 0; i < threads_num; ++i) {
        for (int j = 0; j < cell_width; ++j) {
          out << "-";
        }
        out << "*";
      }
      out << "\n";
    };
    auto print_spaces = [&out](int count) {
      for (int i = 0; i < count; ++i) {
        out << " ";
      }
    };

    int spaces = 7;
    print_spaces(spaces);
    print_separator();

    // Header.
    print_spaces(spaces);
    out << "|";
    for (int i = 0; i < threads_num; ++i) {
      int rest = cell_width - 1 /*T*/ - to_string(i).size();
      print_spaces(rest / 2);
      out << "T" << mapping[i];
      print_spaces(rest - rest / 2);
      out << "|";
    }
    out << "\n";

    print_spaces(spaces);
    print_separator();

    auto print_empty_cell = [&]() {
      print_spaces(cell_width);
      out << "|";
    };

    std::map<CoroBase*, int> index;
    std::vector<int> co_depth(threads_num, 0);
    // Rows.
    for (const auto& i : result) {
      int num = inverse_mapping[i.first];
      FitPrinter fp{out, cell_width};
      auto visitor =
          Overloads{[&](std::reference_wrapper<Task> act) {
                      auto base = act.get().get();
                      if (index.find(base) == index.end()) {
                        int sz = index.size();
                        index[base] = sz;
                      }
                      int length = std::to_string(index[base]).size();
                      out << index[base];
                      assert(spaces - length >= 0);
                      print_spaces(7 - length);
                      out << "|";
                      for (int j = 0; j < num; ++j) {
                        print_empty_cell();
                      }
                      fp.Out(" ");
                      // std::cerr << "writing " << &act.get() << "\n";
                      fp.Out(std::string{act.get()->GetName()});
                      fp.Out("(");
                      const auto& args = act.get()->GetStrArgs();
                      for (int i = 0; i < args.size(); ++i) {
                        if (i > 0) {
                          fp.Out(", ");
                        }
                        fp.Out(args[i]);
                      }
                      fp.Out(")");
                    },
                    [&](const CoroutineStatus& cor) {
                      print_spaces(7);
                      out << "|";
                      for (int j = 0; j < num; ++j) {
                        print_empty_cell();
                      }
                      auto print_formated_spaces = [&fp](int count) {
                        for (int i = 0; i < count; ++i) {
                          fp.Out(" ");
                        }
                      };
                      if (cor.has_started) {
                        print_formated_spaces(co_depth[num] + 1);
                        fp.Out(">");
                        co_depth[num]++;
                      } else {
                        print_formated_spaces(co_depth[num]);
                        fp.Out("<");
                        co_depth[num]--;
                      }
                      fp.Out(cor.name);
                    },
                    [&](const CreateNewThreadHistoryInfo& new_thread) {
                      print_spaces(7);
                      out << "|";
                      for (int j = 0; j < num; ++j) {
                        print_empty_cell();
                      }
                      fp.Out(std::string(" ->T") +
                             std::to_string(new_thread.created_thread_id));
                    },
                    [&](const WaitThreadInfo& wait_thread){
                      print_spaces(7);
                      out << "|";
                      for (int j = 0; j < num; ++j) {
                        print_empty_cell();
                      }
                      fp.Out(std::string(" <-"));
                    }

          };
      std::visit(visitor, i.second);
      assert(fp.rest > 0 && "increase cell_width in pretty printer");
      print_spaces(fp.rest);
      out << "|";

      for (int j = 0; j < threads_num - num - 1; ++j) {
        print_empty_cell();
      }
      out << "\n";
    }

    print_spaces(spaces);
    print_separator();
  }

 private:
  // Counts how much symbols is left after printing.
  template <typename Out_t>
  struct FitPrinter {
    Out_t& out;
    int rest;
    FitPrinter(Out_t& out, int rest) : out(out), rest(rest) {}

    void Out(const std::string_view& msg) {
      rest -= msg.size();
      out << msg;
    }
  };
  size_t threads_num;
};
