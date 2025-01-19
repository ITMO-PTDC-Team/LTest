#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

using namespace llvm;

using Builder = IRBuilder<>;

using FunIndex = std::set<std::pair<StringRef, StringRef>>;

const StringRef nonatomic_attr = "ltest_nonatomic";

FunIndex CreateFunIndex(const Module &M) {
  FunIndex index{};
  for (auto it = M.global_begin(); it != M.global_end(); ++it) {
    if (it->getName() != "llvm.global.annotations") {
      continue;
    }
    auto *CA = dyn_cast<ConstantArray>(it->getOperand(0));
    for (auto o_it = CA->op_begin(); o_it != CA->op_end(); ++o_it) {
      auto *CS = dyn_cast<ConstantStruct>(o_it->get());
      auto *fun = dyn_cast<Function>(CS->getOperand(0));
      auto *AnnotationGL = dyn_cast<GlobalVariable>(CS->getOperand(1));
      auto annotation =
          dyn_cast<ConstantDataArray>(AnnotationGL->getInitializer())
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
  YieldInserter(Module &M) : M(M) {
    CoroYieldF = M.getOrInsertFunction(
        "CoroYield", FunctionType::get(Type::getVoidTy(M.getContext()), {}));
  }

  void Run(const FunIndex &index) {
    for (auto &F : M) {
      if (IsTarget(F.getName(), index)) {
        InsertYields(F, index);

        errs() << "yields inserted to the " << F.getName() << "\n";
        errs() << F << "\n";
      }
    }
  }

 private:
  bool IsTarget(const StringRef fun_name, const FunIndex &index) {
    return HasAttribute(index, fun_name, nonatomic_attr);
  }

  bool NeedInterrupt(Instruction *insn, const FunIndex &index) {
    if (isa<LoadInst>(insn) || isa<StoreInst>(insn) ||
         isa<AtomicRMWInst>(insn) /*||
        isa<InvokeInst>(insn)*/) {
      return true;
    }
    return false;
  }

  void InsertYields(Function &F, const FunIndex &index) {
    auto name = F.getName();
    if (visited.find(name) != visited.end()) {
      return;
    }
    visited.insert(name);
    
    Builder Builder(&*F.begin());
    for (auto &B : F) {
      for (auto it = B.begin(); std::next(it) != B.end(); ++it) {
        if (NeedInterrupt(&*it, index) && !ItsYieldInst(&*std::next(it))) {
          Builder.SetInsertPoint(&*std::next(it));
          Builder.CreateCall(CoroYieldF, {})->getIterator();
          ++it;
        }
      }
    }

    for (auto &B : F) {
      for (auto &I : B) {
        if (auto call = dyn_cast<CallInst>(&I)) {
          auto fun = call->getCalledFunction();
          if (fun && !fun->isDeclaration()) {
            InsertYields(*fun, index);
          }
        }
        if (auto invoke = dyn_cast<InvokeInst>(&I)) {
          auto fun = invoke->getCalledFunction();
          if (fun && !fun->isDeclaration()) {
            InsertYields(*fun, index);
          }
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
  std::set<StringRef> visited{};
};

namespace {

struct YieldInsertPass final : public PassInfoMixin<YieldInsertPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
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
          .RegisterPassBuilderCallbacks = [](PassBuilder &PB) {
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel Level) {
                  MPM.addPass(YieldInsertPass());
                });
          }};
}