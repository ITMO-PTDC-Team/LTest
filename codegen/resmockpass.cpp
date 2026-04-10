#include <llvm/Demangle/Demangle.h>
#include <llvm/IR/Verifier.h>

#include <functional>
#include <map>
#include <utility>

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

using namespace llvm;
// This pass should be used without dynamic libs

// ignore handling by exceptions memory allocation failure. This is very
// unlikely case and shouldn't be in tested program.
// Supporting this case would require -fexcept
constexpr std::string_view ltest_allocation_fun_name = "LtestMemAlloc";
constexpr std::string_view ltest_deallocation_fun_name = "LtestMemDealloc";

SmallVector<Value*> collectArgs(CallBase& call, size_t n = -1) {
  SmallVector<Value*> res_args;
  for (auto& arg : call.args()) {
    if (n == 0) {
      continue;
    }
    res_args.push_back(arg);
    n--;
  }
  return res_args;
}

class ResMockInserter {
 public:
  explicit ResMockInserter(Module& m) : m(m) {
    auto& context = m.getContext();
    auto ptr_type = PointerType::get(context, 0);

    ltest_allocation_fun = m.getOrInsertFunction(
        ltest_allocation_fun_name,
        FunctionType::get(ptr_type, {IntegerType::get(context, 64)}, false));
    ltest_deallocation_fun = m.getOrInsertFunction(
        ltest_deallocation_fun_name,
        FunctionType::get(Type::getVoidTy(context), {ptr_type}, false));
  }
  void Run() {
    IRBuilder<> builder(m.getContext());
    for (auto& f : m) {
      // dirty hack, but otherwise boost contextes allocation is also mocked,
      // which produces warning by asan about double free whichh i couldn't
      // debug
      std::string demangled_parent = demangle(f.getName());
      if (demangled_parent.find("boost::context") != std::string::npos) {
        continue;
      }
      for (auto& bb : f) {
        for (auto& in : bb) {
          if (!isa<CallBase>(&in)) {
            continue;
          }
          CallBase& call = *dyn_cast<CallBase>(&in);
          auto called_fun = call.getCalledFunction();
          if (!called_fun) {
            continue;
          }
          std::string demangled = demangle(called_fun->getName());
          auto it = replaces.find(demangled);
          if (it == replaces.end()) {
            continue;
          }
          auto [rep_callee, rep_args] = (it->second)(call);
          builder.SetInsertPoint(&in);
          CallBase* rep;
          if (auto* invoke = dyn_cast<InvokeInst>(&call)) {
            rep = builder.CreateInvoke(rep_callee, invoke->getNormalDest(),
                                       invoke->getUnwindDest(), rep_args);
          } else {
            rep = builder.CreateCall(rep_callee, rep_args);
          }

          call.replaceAllUsesWith(rep);
          to_delete.push_back(&call);
        }
      }
    }

    for (auto& el : to_delete) {
      el->eraseFromParent();
    }
  }

 private:
  FunctionCallee ltest_allocation_fun, ltest_deallocation_fun;
  Module& m;
  // to avoid problems with correct iteration through instrcution remove
  // instructions only at the end
  std::vector<CallBase*> to_delete;
  std::map<
      std::string_view,
      std::function<std::pair<FunctionCallee, SmallVector<Value*>>(CallBase&)>>
      replaces = {
          {"malloc",
           [this](CallBase& a) -> auto {
             assert(a.arg_size() == 1 && "args count is 1");
             return std::pair{ltest_allocation_fun, collectArgs(a)};
           }},
          {"operator new(unsigned long)",
           [this](CallBase& a) -> auto {
             assert(a.arg_size() == 1 && "args count is 1");
             return std::pair{ltest_allocation_fun, collectArgs(a)};
           }},
          {"operator new[](unsigned long)",
           [this](CallBase& a) -> auto {
             assert(a.arg_size() == 1 && "args count is 1");
             return std::pair{ltest_allocation_fun, collectArgs(a)};
           }},
          {"free",
           [this](CallBase& a) -> auto {
             assert(a.arg_size() == 1 && "args count is 1");
             return std::pair{ltest_deallocation_fun, collectArgs(a)};
           }},
          {"operator delete(void*, unsigned long)",
           [this](CallBase& a) -> auto {
             assert(a.arg_size() == 2 && "args count is 2");
             return std::pair{ltest_deallocation_fun, collectArgs(a, 1)};
           }},
          {"operator delete[](void*, unsigned long)",
           [this](CallBase& a) -> auto {
             assert(a.arg_size() == 2 && "args count is 2");
             return std::pair{ltest_deallocation_fun, collectArgs(a, 1)};
           }}};
};
namespace {

struct ResMockPass final : public PassInfoMixin<ResMockPass> {
  PreservedAnalyses run(Module& M, ModuleAnalysisManager& AM) {
    ResMockInserter inserter(M);
    inserter.Run();
    if (verifyModule(M, &errs())) {
      report_fatal_error("module verification failed", false);
    }
    return PreservedAnalyses::none();
  };
};

}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {.APIVersion = LLVM_PLUGIN_API_VERSION,
          .PluginName = "resmockpass",
          .PluginVersion = "v0.1",
          .RegisterPassBuilderCallbacks = [](PassBuilder& pb) {
            // This parsing we need for testing with opt
            pb.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager& mpm,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "resmock") {
                    mpm.addPass(ResMockPass());
                    return true;
                  }
                  return false;
                });
            pb.registerPipelineStartEPCallback(
                [](ModulePassManager& mpm, OptimizationLevel level) {
                  mpm.addPass(ResMockPass());
                });
          }};
}
