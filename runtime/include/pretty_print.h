#pragma once
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

using FullHistoryWithThreads =
    std::vector<std::pair<int, std::variant<std::reference_wrapper<Task>, CoroutineStatus>>>;

struct PrettyPrinter {
  PrettyPrinter(size_t threads_num);

  // -------------------------
  // 1) Regular history printer: Invoke/Response
  // -------------------------
  template <typename Out_t>
  void PrettyPrint(const std::vector<std::variant<Invoke, Response>>& result,
                   Out_t& out) {
    auto get_thread_num = [](const std::variant<Invoke, Response>& v) {
      if (v.index() == 0) return std::get<0>(v).thread_id;
      return std::get<1>(v).thread_id;
    };

    int cell_width = 50;

    auto print_separator = [&out, this, cell_width]() {
      out << "*";
      for (int i = 0; i < static_cast<int>(threads_num); ++i) {
        for (int j = 0; j < cell_width; ++j) out << "-";
        out << "*";
      }
      out << "\n";
    };

    auto print_spaces = [&out](int count) {
      for (int i = 0; i < count; ++i) out << " ";
    };

    print_separator();
    out << "|";
    for (int i = 0; i < static_cast<int>(threads_num); ++i) {
      int rest = cell_width - 1 - static_cast<int>(to_string(i).size());
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
      for (int j = 0; j < num; ++j) print_empty_cell();

      FitPrinter fp{out, cell_width};
      fp.Out(" ");
      if (ev.index() == 0) {
        auto inv = std::get<0>(ev);
        auto& task = inv.GetTask();
        fp.Out("[" + std::to_string(task->GetId()) + "] ");
        fp.Out(std::string{task->GetName()});
        fp.Out("(");
        const auto& args = task->GetStrArgs();
        for (int i = 0; i < static_cast<int>(args.size()); ++i) {
          if (i > 0) fp.Out(", ");
          fp.Out(args[i]);
        }
        fp.Out(")");
      } else {
        auto resp = std::get<1>(ev);
        fp.Out("<-- " + to_string(resp.GetTask()->GetRetVal()));
      }

      assert(fp.rest > 0 && "increase cell_width in pretty printer");
      print_spaces(fp.rest);
      out << "|";

      for (int j = 0; j < static_cast<int>(threads_num) - num - 1; ++j)
        print_empty_cell();
      out << "\n";
    }

    print_separator();
  }

  // -------------------------
  // 2) Dual history printer: DualHistoryEvent
  // -------------------------
  template <typename Out_t>
  void PrettyPrint(const std::vector<DualHistoryEvent>& result, Out_t& out) {
    auto get_thread_num = [](const DualHistoryEvent& v) -> int {
      return std::visit([](auto const& ev) { return ev.thread_id; }, v);
    };

    int cell_width = 50;

    auto print_separator = [&out, this, cell_width]() {
      out << "*";
      for (int i = 0; i < static_cast<int>(threads_num); ++i) {
        for (int j = 0; j < cell_width; ++j) out << "-";
        out << "*";
      }
      out << "\n";
    };

    auto print_spaces = [&out](int count) {
      for (int i = 0; i < count; ++i) out << " ";
    };

    print_separator();
    out << "|";
    for (int i = 0; i < static_cast<int>(threads_num); ++i) {
      int rest = cell_width - 1 - static_cast<int>(to_string(i).size());
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
      for (int j = 0; j < num; ++j) print_empty_cell();

      FitPrinter fp{out, cell_width};
      fp.Out(" ");

      std::visit([&](auto const& e) {
        using E = std::decay_t<decltype(e)>;

        // Ordinary events
        if constexpr (std::is_same_v<E, Invoke>) {
          auto& task = e.GetTask();
          fp.Out("[" + std::to_string(task->GetId()) + "] ");
          fp.Out(std::string{task->GetName()});
          fp.Out("(");
          const auto& args = task->GetStrArgs();
          for (int i = 0; i < static_cast<int>(args.size()); ++i) {
            if (i > 0) fp.Out(", ");
            fp.Out(args[i]);
          }
          fp.Out(")");
        } else if constexpr (std::is_same_v<E, Response>) {
          fp.Out("<-- " + to_string(e.GetTask()->GetRetVal()));
        }

        // Dual events
        else if constexpr (std::is_same_v<E, RequestInvoke>) {
          auto& task = e.GetTask();
          fp.Out("[" + std::to_string(task->GetId()) + "] REQ ");
          fp.Out(std::string{task->GetName()});
          fp.Out("(");
          const auto& args = task->GetStrArgs();
          for (int i = 0; i < static_cast<int>(args.size()); ++i) {
            if (i > 0) fp.Out(", ");
            fp.Out(args[i]);
          }
          fp.Out(")");
        } else if constexpr (std::is_same_v<E, RequestResponse>) {
          fp.Out("<-- request_done");
        } else if constexpr (std::is_same_v<E, FollowUpInvoke>) {
          fp.Out("FOLLOWUP");
        } else if constexpr (std::is_same_v<E, FollowUpResponse>) {
          fp.Out("<-- " + to_string(e.result));
        } else {
          fp.Out("<?>");
        }
      }, ev);

      assert(fp.rest > 0 && "increase cell_width in pretty printer");
      print_spaces(fp.rest);
      out << "|";

      for (int j = 0; j < static_cast<int>(threads_num) - num - 1; ++j)
        print_empty_cell();
      out << "\n";
    }

    print_separator();
  }

  // -------------------------
  // 3) FullHistoryWithThreads (existing)
  // -------------------------
  template <typename Out_t>
  void PrettyPrint(FullHistoryWithThreads& result, Out_t& out) {
    int cell_width = 20;

    auto print_separator = [&out, this, cell_width]() {
      out << "*";
      for (int i = 0; i < static_cast<int>(threads_num); ++i) {
        for (int j = 0; j < cell_width; ++j) out << "-";
        out << "*";
      }
      out << "\n";
    };
    auto print_spaces = [&out](int count) {
      for (int i = 0; i < count; ++i) out << " ";
    };

    int spaces = 7;
    print_spaces(spaces);
    print_separator();

    print_spaces(spaces);
    out << "|";
    for (int i = 0; i < static_cast<int>(threads_num); ++i) {
      int rest = cell_width - 1 - static_cast<int>(to_string(i).size());
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
        std::cout << index[base];
        assert(spaces - length >= 0);
        print_spaces(7 - length);
        out << "|";
        for (int j = 0; j < num; ++j) print_empty_cell();
        fp.Out(" ");
        fp.Out(std::string{act.get()->GetName()});
        fp.Out("(");
        const auto& args = act.get()->GetStrArgs();
        for (int k = 0; k < static_cast<int>(args.size()); ++k) {
          if (k > 0) fp.Out(", ");
          fp.Out(args[k]);
        }
        fp.Out(")");
      } else if (i.second.index() == 1) {
        print_spaces(7);
        out << "|";
        for (int j = 0; j < num; ++j) print_empty_cell();
        auto cor = std::get<1>(i.second);
        auto print_formated_spaces = [&fp](int count) {
          for (int k = 0; k < count; ++k) fp.Out(" ");
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
        assert(fp.rest > 0 && "increase cell_width in pretty printer");
      }
      print_spaces(fp.rest);
      out << "|";

      for (int j = 0; j < static_cast<int>(threads_num) - num - 1; ++j)
        print_empty_cell();
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
      rest -= static_cast<int>(msg.size());
      out << msg;
    }
  };

  size_t threads_num;
};