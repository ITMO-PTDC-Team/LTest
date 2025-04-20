#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

using namespace llvm;

using Builder = IRBuilder<>;

using FunIndex = std::set<std::pair<StringRef, StringRef>>;

const StringRef NonatomicAttr = "ltest_nonatomic";

FunIndex CreateFunIndex(const Module &m) {
  FunIndex index{};
  for (auto it = m.global_begin(); it != m.global_end(); ++it) {
    if (it->getName() != "llvm.global.annotations") {
      continue;
    }
    auto *ca = dyn_cast<ConstantArray>(it->getOperand(0));
    for (auto o_it = ca->op_begin(); o_it != ca->op_end(); ++o_it) {
      auto *cs = dyn_cast<ConstantStruct>(o_it->get());
      auto *fun = dyn_cast<Function>(cs->getOperand(0));
      auto *annotation_gl = dyn_cast<GlobalVariable>(cs->getOperand(1));
      auto annotation =
          dyn_cast<ConstantDataArray>(annotation_gl->getInitializer())
              ->getAsCString();
      index.insert({annotation, fun->getName()});
    }
  }
  return index;
}

bool HasAttribute(const FunIndex &index, const StringRef name,
                  const StringRef attr) {
  return index.find({attr, name}) != index.end();
}

struct YieldInserter {
  YieldInserter(Module &m) : M(m) {
    CoroYieldF = m.getOrInsertFunction(
        "CoroYield", FunctionType::get(Type::getVoidTy(m.getContext()), {}));
  }

  void Run(const FunIndex &index) {
    for (auto &f : M) {
      if (IsTarget(f.getName(), index)) {
        InsertYields(f, index);

        errs() << "yields inserted to the " << f.getName() << "\n";
        errs() << f << "\n";
      }
    }
  }

 private:
  bool IsTarget(const StringRef fun_name, const FunIndex &index) {
    return HasAttribute(index, fun_name, NonatomicAttr);
  }

  bool NeedInterrupt(Instruction *insn, const FunIndex &index) {
    if (isa<LoadInst>(insn) || isa<StoreInst>(insn) ||
         isa<AtomicRMWInst>(insn) /*||
        isa<InvokeInst>(insn)*/) {
      return true;
    }
    return false;
  }

  void InsertYields(Function &f, const FunIndex &index) {
    Builder builder(&*f.begin());
    for (auto &b : f) {
      for (auto it = b.begin(); std::next(it) != b.end(); ++it) {
        if (NeedInterrupt(&*it, index) && !ItsYieldInst(&*std::next(it))) {
          builder.SetInsertPoint(&*std::next(it));
          builder.CreateCall(CoroYieldF, {})->getIterator();
          ++it;
        }
      }
    }
  }

  bool ItsYieldInst(Instruction *inst) {
    if (auto call = dyn_cast<CallInst>(inst)) {
      if (auto fun = call->getCalledFunction()) {
        if (fun->hasName() &&
            fun->getName() == CoroYieldF.getCallee()->getName()) {
          return true;
        }
      }
    }
    return false;
  }

  Module &M;
  FunctionCallee CoroYieldF;
};

namespace {

struct YieldInsertPass final : public PassInfoMixin<YieldInsertPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {  // NOLINT
    auto fun_index = CreateFunIndex(M);

    YieldInserter gen{M};
    gen.Run(fun_index);

    return PreservedAnalyses::none();
  };
};

}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {.APIVersion = LLVM_PLUGIN_API_VERSION,
          .PluginName = "yield_insert",
          .PluginVersion = "v0.1",
          .RegisterPassBuilderCallbacks = [](PassBuilder &pb) {
            pb.registerPipelineStartEPCallback(
                [](ModulePassManager &mpm, OptimizationLevel level) {
                  mpm.addPass(YieldInsertPass());
                });
          }};
}