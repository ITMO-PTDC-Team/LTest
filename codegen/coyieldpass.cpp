#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>

#include <fstream>
#include <ios>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

#include "llvm/Demangle/Demangle.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/Cloning.h"
using namespace llvm;
using Builder = IRBuilder<>;

constexpr std::string_view costatus_change = "CoroutineStatusChange";

constexpr std::string_view co_expr_start = "::await_ready";

constexpr std::string_view co_expr_end = "::await_resume";
constexpr std::string_view co_final_suspend = "::final_suspend()";

constexpr std::string_view coroutines_list = "coroutines_list.txt";

constexpr std::string_view parent_list = "parent_list.txt";

constexpr std::string_view no_filter = "any";

static cl::opt<std::string> input_list(
    "coroutine-file", cl::desc("Specify path to file with coroutines to check"),
    llvm::cl::Required);
;
static cl::opt<bool> print_all(
    "print-all",
    cl::desc("Print list of all parent of coroutines to" +
             std::string(parent_list) + " and coroutines to" +
             std::string(coroutines_list)),
    cl::init(true));

struct ConfigEntry {
  ConfigEntry(const std::optional<std::string> &parent_name,
              const std::optional<std::string> &co_name,
              const std::string &print_name)
      : parent_name(parent_name), co_name(co_name), print_name(print_name) {};
  std::optional<std::string> parent_name;
  std::optional<std::string> co_name;
  std::string print_name;
};
struct CoYieldInserter {
  CoYieldInserter(Module &M, std::vector<ConfigEntry> &&white_list,
                  std::optional<std::pair<std::fstream, std::fstream>> &&out)
      : M(M), config(std::move(white_list)), files_with_list(std::move(out)) {
    auto &context = M.getContext();
    CoroYieldF = M.getOrInsertFunction(
        costatus_change,
        FunctionType::get(Type::getVoidTy(context),
                          {PointerType::get(Type::getInt8Ty(context), 0),
                           Type::getInt8Ty(context)},
                          {}));
  }

  void Run(const Module &index) {
    for (auto &F : M) {
      std::string demangled = demangle(F.getName());
      auto filt =
          config | std::ranges::views::filter(
                       [&demangled](const ConfigEntry &a) -> bool {
                         return !a.parent_name || a.parent_name == demangled;
                       });
      if (!filt.empty()) {
        InsertYields(filt, F);
      }
    }
  }

 private:
  void InsertYields(auto filtered_config, Function &f) {
    Builder builder(&*f.begin());
    for (auto &b : f) {
      for (auto &i : b) {
        CallInst *call = dyn_cast<CallInst>(&i);
        if (call) {
          auto fn = call->getCalledFunction();
          if (fn == nullptr) {
            continue;
          }
          auto raw_fn_name = fn->getName();
          std::string co_name = demangle(raw_fn_name);
          if (files_with_list.has_value()) {
            files_with_list->second << co_name << "\n";
          }
          auto start = co_name.find(co_expr_start);
          if (start != std::string::npos) {
            if (i.getPrevNode()) {
              auto prev = dyn_cast<CallInst>(i.getPrevNode());
              if (prev) {
                auto p = prev->getCalledFunction();
                if (p != nullptr) {
                  auto raw_prev_name = p->getName();
                  std::string prev_name = demangle(raw_prev_name);
                  if (prev_name.find(co_final_suspend) != std::string::npos) {
                    continue;
                  }
                }
              }
            }
            builder.SetInsertPoint(call->getNextNode());
            ReplaceCall(filtered_config, co_name, builder, true, start);
            continue;
          }
          auto end_pos = co_name.find(co_expr_end);
          if (end_pos != std::string::npos) {
            builder.SetInsertPoint(call);
            ReplaceCall(filtered_config, co_name, builder, false, end_pos);
            continue;
          }
        }
      }
    }
  }

  void ReplaceCall(auto filtered_config, StringRef co_name, Builder &builder,
                   bool start, int end_pos) {
    auto res_config =
        filtered_config |
        std::ranges::views::filter(
            [&end_pos, &co_name](const ConfigEntry &a) -> bool {
              return !a.co_name || a.co_name == co_name.substr(0, end_pos);
            });
    if (res_config.empty()) {
      return;
    }
    // First in the config will match
    auto first_match = res_config.front();
    errs() << "replaced " << co_name.str() << "\n";
    auto llvm_start =
        ConstantInt::get(Type::getInt1Ty(builder.getContext()), start);
    Constant *str_const = ConstantDataArray::getString(
        M.getContext(), first_match.print_name, true);
    auto zero = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0);
    Constant *ind[] = {zero, zero};
    GlobalVariable *global = new GlobalVariable(
        M, str_const->getType(), true, GlobalValue::PrivateLinkage, str_const);
    auto ptr =
        ConstantExpr::getGetElementPtr(global->getValueType(), global, ind);
    builder.CreateCall(CoroYieldF, {ptr, llvm_start});
  }

  Module &M;
  FunctionCallee CoroYieldF;
  std::vector<ConfigEntry> config;
  std::optional<std::pair<std::fstream, std::fstream>> files_with_list;
};

namespace {

struct CoYieldInsertPass final : public PassInfoMixin<CoYieldInsertPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    if (input_list.empty()) {
      report_fatal_error("No file  with coroutines list");
    }

    std::fstream input(input_list);
    if (!input.is_open()) {
      report_fatal_error(
          StringRef("Failed to open file with coroutines list: " + input_list));
    }

    std::string line;
    size_t state = 0;
    std::vector<ConfigEntry> config;
    std::optional<std::string> parent_func;
    std::optional<std::string> co_func;
    std::string name;
    while (std::getline(input, line)) {
      if (!line.empty() && !line.starts_with("//")) {
        switch (state) {
          case 0:
            parent_func.reset();
            if (line != no_filter) {
              parent_func = line;
            }
            break;
          case 1:
            co_func.reset();
            if (line != no_filter) {
              co_func = line;
            }
            break;
          case 2:
            name = line;
            config.emplace_back(parent_func, co_func, name);
            break;
          default:
            report_fatal_error("invalid config");
        }
        state = (state + 1) % 3;
      }
    }
    assert(state == 0);
    input.close();
    std::optional<std::pair<std::fstream, std::fstream>> out_files;
    if (print_all) {
      auto mode = std::ios_base::in | std::ios_base::out | std::ios_base::app;
      out_files = {std::fstream(parent_list.data(), mode),
                   std::fstream(coroutines_list.data(), mode)};
    }
    CoYieldInserter gen{M, std::move(config), std::move(out_files)};
    gen.Run(M);

    return PreservedAnalyses::none();
  };
};

}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {.APIVersion = LLVM_PLUGIN_API_VERSION,
          .PluginName = "coyield_insert",
          .PluginVersion = "v0.1",
          .RegisterPassBuilderCallbacks = [](PassBuilder &PB) {
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel Level) {
                  std::set<std::string> l;
                  MPM.addPass(CoYieldInsertPass());
                });
          }};
}