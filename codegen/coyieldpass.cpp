#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/raw_ostream.h>

#include <fstream>
#include <string>
#include <string_view>

#include "llvm/Demangle/Demangle.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/Cloning.h"
using namespace llvm;
using Builder = IRBuilder<>;

constexpr std::string_view costatus_change = "CoroutineStatusChange";
constexpr std::string_view initial_suspend = "::initial_suspend()";
constexpr std::string_view initial_suspend_const = "::initial_suspend() const";

constexpr std::string_view final_suspend = "::final_suspend()";
constexpr std::string_view final_suspend_const = "::final_suspend() const";

static cl::opt<std::string> input_list(
    "coroutine-file", cl::desc("Specify path to file with coroutines to check"),
    llvm::cl::Required);
;

struct CoYieldInserter {
  CoYieldInserter(Module &M, std::set<std::string> &&white_list)
      : M(M), white_list(std::move(white_list)) {
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
      InsertYields(F);
    }
  }

 private:
  void InsertYields(Function &f) {
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
          if (co_name.ends_with(initial_suspend) ||
              co_name.ends_with(initial_suspend_const)) {
            builder.SetInsertPoint(call->getNextNode());
            ReplaceCall(call, co_name, builder, true);
            continue;
          }
          if (co_name.ends_with(final_suspend) ||
              co_name.ends_with(final_suspend_const)) {
            builder.SetInsertPoint(call);
            ReplaceCall(call, co_name, builder, false);
            continue;
          }
        }
      }
    }
  }

  void ReplaceCall(CallInst *call, StringRef co_name, Builder &builder,
                   bool start) {
    auto par = demangle(call->getParent()->getParent()->getName());
        if (!white_list.contains(par)) {
      return;
    }
    errs() << "replaced " << par <<"\n";
    errs().flush();
    auto llvm_start =
        ConstantInt::get(Type::getInt1Ty(builder.getContext()), start);
    Constant *str_const =
        ConstantDataArray::getString(M.getContext(), par, true);
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
  std::set<std::string> white_list;
};

namespace {

struct CoYieldInsertPass final : public PassInfoMixin<CoYieldInsertPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    if (input_list.empty()) {
      report_fatal_error("No file  with coroutines list");
    }

    std::ifstream input(input_list);
    if (!input.is_open()) {
      report_fatal_error(
          StringRef("Failed to open file with coroutines list: " + input_list));
    }

    std::string line;
    std::set<std::string> white_list;
    while (std::getline(input, line)) {
      if (!line.starts_with("//")) {
        white_list.insert(line);
      }
    }
    input.close();
    CoYieldInserter gen{M, std::move(white_list)};
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