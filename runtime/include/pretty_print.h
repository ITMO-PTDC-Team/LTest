#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

#include "lib.h"
#include "lincheck.h"
#include "lincheck_dual.h"
#include "logger.h"

using std::string;
using std::to_string;

using FullHistoryWithThreads = std::vector<std::pair<
    int, std::variant<std::reference_wrapper<Task>, CoroutineStatus>>>;

struct PrettyPrinter {
  explicit PrettyPrinter() = default;

  template <typename Out_t>
  void PrettyPrint(const std::vector<std::variant<Invoke, Response>>& result,
                   int threads_num, Out_t& out) {
    auto get_thread_num = [](const std::variant<Invoke, Response>& v) {
      if (v.index() == 0) {
        return get<0>(v).thread_id;
      }
      return get<1>(v).thread_id;
    };

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
        for (int j = 0; j < static_cast<int>(args.size()); ++j) {
          if (j > 0) {
            fp.Out(", ");
          }
          fp.Out(args[j]);
        }
        fp.Out(")");
      } else {
        auto resp = get<1>(i);
        fp.Out("<-- " + to_string(resp.result));
      }
      print_spaces(std::max(fp.rest, 0));
      out << "|";

      for (int j = 0; j < threads_num - num - 1; ++j) {
        print_empty_cell();
      }
      out << "\n";
    }

    print_separator();
  }

  template <typename Out_t>
  void PrettyPrint(const std::vector<DualHistoryEvent>& result, int threads_num,
                   Out_t& out) {
    auto get_thread_num = [](const DualHistoryEvent& v) -> int {
      return std::visit([](auto const& ev) { return ev.thread_id; }, v);
    };

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

    for (const auto& ev : result) {
      int num = get_thread_num(ev);
      out << "|";
      for (int j = 0; j < num; ++j) {
        print_empty_cell();
      }

      FitPrinter fp{out, cell_width};
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

      print_spaces(std::max(fp.rest, 0));
      out << "|";

      for (int j = 0; j < threads_num - num - 1; ++j) {
        print_empty_cell();
      }
      out << "\n";
    }

    print_separator();
  }

  template <typename Out_t>
  void PrettyPrint(FullHistoryWithThreads& result, int threads_num,
                   Out_t& out) {
    int cell_width = 20;

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

    int spaces = 7;
    print_spaces(spaces);
    print_separator();

    print_spaces(spaces);
    out << "|";
    for (int i = 0; i < threads_num; ++i) {
      int rest = cell_width - 1 - to_string(i).size();
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
    std::vector<int> co_depth(threads_num, 0);
    for (const auto& i : result) {
      int num = i.first;
      FitPrinter fp{out, cell_width};
      if (i.second.index() == 0) {
        auto act = std::get<0>(i.second);
        auto base = act.get().get();
        if (index.find(base) == index.end()) {
          int sz = static_cast<int>(index.size());
          index[base] = sz;
        }
        int length = static_cast<int>(std::to_string(index[base]).size());
        out << index[base];
        assert(spaces - length >= 0);
        print_spaces(7 - length);
        out << "|";
        for (int j = 0; j < num; ++j) {
          print_empty_cell();
        }
        fp.Out(" ");
        PrintMethodCall(act.get(), fp);
      } else if (i.second.index() == 1) {
        print_spaces(7);
        out << "|";
        for (int j = 0; j < num; ++j) {
          print_empty_cell();
        }
        auto cor = std::get<1>(i.second);
        auto print_formated_spaces = [&fp](int count) {
          for (int j = 0; j < count; ++j) {
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
      }
      print_spaces(std::max(fp.rest, 0));
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
