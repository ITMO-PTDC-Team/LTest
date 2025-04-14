#pragma once
#include <cassert>
#include <iostream>
#include <string>
#include <variant>
#include <vector>

#include "lib.h"
#include "lincheck.h"
#include "logger.h"

using std::string;
using std::to_string;
using FullHistoryWithThreads = std::vector<std::pair<
    int, std::variant<std::reference_wrapper<Task>, CoroutineStatus>>>;
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
  void PrettyPrint(FullHistoryWithThreads& result, Out_t& out) {
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

    int spaces = 7;
    print_spaces(spaces);
    print_separator();

    // Header.
    print_spaces(spaces);
    out << "|";
    for (int i = 0; i < threads_num; ++i) {
      int rest = cell_width - 1 /*T*/ - to_string(i).size();
      print_spaces(rest / 2);
      out << "T" << i;
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

    // Rows.
    for (const auto& i : result) {
      int num = i.first;
      FitPrinter fp{out, cell_width};
      if (i.second.index() == 0) {
        auto act = std::get<0>(i.second);
        auto base = act.get().get();
        if (index.find(base) == index.end()) {
          int sz = index.size();
          index[base] = sz;
        }
        int length = std::to_string(index[base]).size();
        std::cout << index[base];
        assert(spaces - length >= 0);
        print_spaces(7 - length);
        out << "|";
        for (int j = 0; j < num; ++j) {
          print_empty_cell();
        }
        fp.Out(" ");
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
      } else if (i.second.index() == 1) {
        print_spaces(7);
        out << "|";
        for (int j = 0; j < num; ++j) {
          print_empty_cell();
        }
        auto cor = std::get<1>(i.second);
        fp.Out(cor.has_started ? ">" : "<");
        auto start = cor.name.find_last_of("::");
        auto end = cor.name.find_last_of("(");
        // to handle namespace in args
        while (start > end) {
          start = cor.name.find_last_of("::", end);
        }
        auto name = std::string(cor.name.substr(start + 1, end - start - 1));
        fp.Out(name);
        // std::cerr << cor.name << "\n";
        assert(fp.rest > 0 && "increase cell_width in pretty printer");
      }
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

    void Out(const std::string& msg) {
      rest -= msg.size();
      out << msg;
    }
  };
  size_t threads_num;
};
