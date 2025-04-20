#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/ObjectYAML/YAML.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>

#include <cassert>
#include <fstream>
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
constexpr std::string_view co_initial_suspend = "::initial_suspend()";
constexpr std::string_view co_final_suspend = "::final_suspend()";

constexpr std::string_view no_filter = "any";

static cl::opt<std::string> input_list(
    "coroutine-file", cl::desc("Specify path to file with coroutines to check"),
    llvm::cl::Required);
;
struct CoroutineFilter {
  CoroutineFilter() = default;
  CoroutineFilter(const std::optional<std::string> &parent_name,
                  const std::optional<std::string> &co_name,
                  const std::string &print_name)
      : parent_name(parent_name), co_name(co_name), print_name(print_name) {};
  std::optional<std::string> parent_name;
  std::optional<std::string> co_name;
  std::string print_name;
};

namespace llvm {
namespace yaml {
template <>
struct MappingTraits<CoroutineFilter> {
  static void mapping(IO &io, CoroutineFilter &config) {//NOLINT
    io.mapRequired("Name", config.print_name);
    io.mapOptional("Coroutine", config.co_name);
    io.mapOptional("Parent", config.parent_name);
  }
};
}  // namespace yaml
}  // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(CoroutineFilter);

struct CoYieldInserter {
  CoYieldInserter(Module &m, std::vector<CoroutineFilter> &&config)
      : m(m), co_filter(std::move(config)) {
    auto &context = m.getContext();
    coroYieldF = m.getOrInsertFunction(
        costatus_change,
        FunctionType::get(Type::getVoidTy(context),
                          {PointerType::get(Type::getInt8Ty(context), 0),
                           Type::getInt8Ty(context)},
                          {}));
  }

  void Run(const Module &index) {
    for (auto &f : m) {
      std::string demangled = demangle(f.getName());
      auto filt =
          co_filter | std::ranges::views::filter(
                       [&demangled](const CoroutineFilter &a) -> bool {
                         return !a.parent_name || a.parent_name == demangled;
                       });
      if (!filt.empty()) {
        InsertYields(filt, f);
      }
    }
  }

 private:
  void InsertYields(auto filtered_config, Function &f) {
    Builder builder(&*f.begin());
    /*
    In fact co_await expr when expr is coroutine is
    co_await initial_suspend()
    coro body...
    co_await_final_suspend()
    We are interested to insert only before initial_suspend and
    after final_suspend
    */
    int skip_insert_points = 0;
    for (auto &b : f) {
      for (auto &i : b) {
        CallInst *call = dyn_cast<CallInst>(&i);
        if (call) {
          auto c_fn = call->getCalledFunction();
          if (c_fn == nullptr) {
            continue;
          }
          auto raw_fn_name = c_fn->getName();
          std::string co_name = demangle(raw_fn_name);
          auto initial = co_name.find(co_initial_suspend);
          if (initial != std::string::npos) {
            builder.SetInsertPoint(call);
            InsertCall(filtered_config, co_name, builder, true, initial);
            skip_insert_points = 2;
            continue;
          }
          auto final = co_name.find(co_final_suspend);
          if (final != std::string::npos) {
            builder.SetInsertPoint(call->getNextNode());
            InsertCall(filtered_config, co_name, builder, false, final);
            skip_insert_points = 2;
            continue;
          }
          auto start = co_name.find(co_expr_start);
          if (start != std::string::npos) {
            if (skip_insert_points != 0) {
              assert(skip_insert_points == 2);
              skip_insert_points--;
              continue;
            }
            builder.SetInsertPoint(call);
            InsertCall(filtered_config, co_name, builder, true, start);
            continue;
          }
          auto end_pos = co_name.find(co_expr_end);
          if (end_pos != std::string::npos) {
            if (skip_insert_points != 0) {
              assert(skip_insert_points == 1);
              skip_insert_points--;
              continue;
            }
            builder.SetInsertPoint(call->getNextNode());
            InsertCall(filtered_config, co_name, builder, false, end_pos);
            continue;
          }
        }
      }
    }
  }

  void InsertCall(auto filtered_config, StringRef co_name, Builder &builder,
                  bool start, int end_pos) {
    auto res_config =
        filtered_config |
        std::ranges::views::filter(
            [&end_pos, &co_name](const CoroutineFilter &a) -> bool {
              return !a.co_name || a.co_name == co_name.substr(0, end_pos);
            });
    if (res_config.empty()) {
      return;
    }
    // First in the config will match
    auto first_match = res_config.front();
    errs() << "inserted " << co_name.str() << "\n";
    auto llvm_start =
        ConstantInt::get(Type::getInt1Ty(builder.getContext()), start);
    Constant *str_const = ConstantDataArray::getString(
        m.getContext(), first_match.print_name, true);
    auto zero = ConstantInt::get(Type::getInt32Ty(m.getContext()), 0);
    Constant *ind[] = {zero, zero};
    GlobalVariable *global = new GlobalVariable(
        m, str_const->getType(), true, GlobalValue::PrivateLinkage, str_const);
    auto ptr =
        ConstantExpr::getGetElementPtr(global->getValueType(), global, ind);
    builder.CreateCall(coroYieldF, {ptr, llvm_start});
  }

  Module &m;
  FunctionCallee coroYieldF;
  std::vector<CoroutineFilter> co_filter;
};

namespace {

struct CoYieldInsertPass final : public PassInfoMixin<CoYieldInsertPass> {
  PreservedAnalyses run(Module &m, ModuleAnalysisManager &am){//NOLINT
    if (input_list.empty()) {
      report_fatal_error("No file  with coroutines list");
    }

    auto file = llvm::MemoryBuffer::getFile(input_list);
    if (!file) {
      report_fatal_error("Failed to load config file\n");
    }

    llvm::yaml::Input input(file.get()->getBuffer());
    std::vector<CoroutineFilter> filt;
    input >> filt;

    if (input.error()) {
      report_fatal_error("Error parsing YAML\n");
    }
    CoYieldInserter gen{m, std::move(filt)};
    gen.Run(m);
    return PreservedAnalyses::none();
  };
};

}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {.APIVersion = LLVM_PLUGIN_API_VERSION,
          .PluginName = "coyield_insert",
          .PluginVersion = "v0.1",
          .RegisterPassBuilderCallbacks = [](PassBuilder &pb) {
            pb.registerPipelineStartEPCallback(
                [](ModulePassManager &mpm, OptimizationLevel level) {
                  std::set<std::string> l;
                  mpm.addPass(CoYieldInsertPass());
                });
          }};
}