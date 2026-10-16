#pragma once

#include <algorithm>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

#include "attempt.h"
#include "lib.h"
#include "lincheck.h"
#include "lincheck_dual.h"

namespace ltest {

using std::string;
using std::to_string;

struct PrettyPrinter {
  explicit PrettyPrinter() = default;

  template <typename Out_t>
  void PrettyPrint(const AttemptReport& result, int threads_num, Out_t& out) {
    std::map<int, size_t> threads;
    for (const auto& op : result.operations) threads[op.id] = op.thread;

    int cell_width = 50;

    auto print_separator = [threads_num, &out, cell_width]() {
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
    out << "|";
    for (int i = 0; i < threads_num; ++i) {
      int rest = cell_width - 1 - to_string(i).size();
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

    for (const auto& ev : result.events) {
      int num = threads.at(ev.operation);
      out << "|";
      for (int j = 0; j < num; ++j) {
        print_empty_cell();
      }

      FitPrinter fp{out, cell_width};
      fp.Out(ev.text);

      print_spaces(std::max(fp.rest, 0));
      out << "|";

      for (int j = 0; j < threads_num - num - 1; ++j) {
        print_empty_cell();
      }
      out << "\n";
    }

    print_separator();
  }

  template <typename Event>
  std::string FormatEvent(const Event& ev) {
    std::ostringstream out;
    FitPrinter fp{out, 50};
    fp.Out(" ");
    std::visit(
        [&](auto const& e) {
          using E = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<E, Invoke>) {
            PrintInvokeCell(e.GetTask(), fp);
          } else if constexpr (std::is_same_v<E, Response>) {
            fp.Out("<-- " + to_string(e.result));
          } else if constexpr (std::is_same_v<E, RequestInvoke>) {
            auto& task = e.GetTask();
            fp.Out("[" + std::to_string(task->GetId()) + "] REQ ");
            PrintMethodCall(task, fp);
          } else if constexpr (std::is_same_v<E, RequestResponse>) {
            fp.Out("<-- request_done");
          } else if constexpr (std::is_same_v<E, FollowUpInvoke>) {
            fp.Out("FOLLOWUP");
          } else if constexpr (std::is_same_v<E, FollowUpResponse>) {
            fp.Out("<-- " + to_string(e.result));
          } else {
            fp.Out("<?>");
          }
        },
        ev);
    return out.str();
  }

 private:
  template <typename Out_t>
  struct FitPrinter {
    Out_t& out;
    int rest;
    FitPrinter(Out_t& out, int rest) : out(out), rest(rest) {}

    void Out(const std::string_view& msg) {
      if (rest <= 0) {
        return;
      }

      if (static_cast<int>(msg.size()) <= rest) {
        rest -= static_cast<int>(msg.size());
        out << msg;
        return;
      }

      if (rest <= 3) {
        for (int i = 0; i < rest; ++i) {
          out << ".";
        }
        rest = 0;
        return;
      }

      out << msg.substr(0, static_cast<std::size_t>(rest - 3)) << "...";
      rest = 0;
    }
  };

  template <typename Out_t>
  static void PrintMethodCall(const Task& task, FitPrinter<Out_t>& fp) {
    fp.Out(std::string{task->GetName()});
    fp.Out("(");
    const auto& args = task->GetStrArgs();
    for (int i = 0; i < static_cast<int>(args.size()); ++i) {
      if (i > 0) {
        fp.Out(", ");
      }
      fp.Out(args[i]);
    }
    fp.Out(")");
  }

  template <typename Out_t>
  static void PrintInvokeCell(const Task& task, FitPrinter<Out_t>& fp) {
    fp.Out("[" + std::to_string(task->GetId()) + "] ");
    PrintMethodCall(task, fp);
  }
};

}  // namespace ltest
