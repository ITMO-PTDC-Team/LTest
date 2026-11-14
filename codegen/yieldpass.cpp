#include <set>

#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Analysis/MemoryLocation.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "mainreplacerpass.h"

using namespace llvm;

using Builder = IRBuilder<>;

using FunIndex = std::set<std::pair<StringRef, StringRef>>;

const StringRef nonatomic_attr = "ltest_nonatomic";
const StringRef atomic_attr = "ltest_atomic";

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
  YieldInserter(Module &M, const std::set<const Instruction *> &skipped_accesses)
      : M(M), skipped_accesses(skipped_accesses) {
    CoroYieldF = M.getOrInsertFunction(
        "CoroYield", FunctionType::get(Type::getVoidTy(M.getContext()), {}));
  }

  void Run(const FunIndex &index) {
    for (auto &F : M) {
      if (IsAtomic(F.getName(), index)) {
        CollectAtomic(F, index);
      }
    }

    for (auto &F : M) {
      if (IsNonAtomic(F.getName(), index)) {
        InsertYields(F, index);
      }
    }
  }

 private:
  bool IsNonAtomic(const StringRef fun_name, const FunIndex &index) {
#if defined(LTEST_EXTERNAL_LAUNCH)
    //TODO can we choose here something wiser?
    return true;
#else
    return HasAttribute(index, fun_name, nonatomic_attr);
#endif
  }

  bool IsAtomic(const StringRef fun_name, const FunIndex &index) {
    return HasAttribute(index, fun_name, atomic_attr);
  }

  bool IsAtomicWrapperCall(Instruction *insn) {
    auto *call = dyn_cast<CallBase>(insn);
    if (call == nullptr) {
      return false;
    }

    auto *fun = call->getCalledFunction();
    if (fun == nullptr) {
      return false;
    }

    const auto name = fun->getName();
    const bool is_atomic_name = name.contains("atomic");
    if (!is_atomic_name) {
      return false;
    }

    return name.contains("load") || name.contains("store") ||
           name.contains("exchange") || name.contains("compare_exchange") ||
           name.contains("fetch_");
  }

  bool NeedInterrupt(Instruction *insn, const FunIndex &index) {
    if (skipped_accesses.contains(insn)) {
      return false;
    }
    if (isa<LoadInst>(insn) || isa<StoreInst>(insn) ||
        isa<AtomicRMWInst>(insn) || isa<AtomicCmpXchgInst>(insn) ||
        IsAtomicWrapperCall(insn) /*||
        isa<InvokeInst>(insn)*/) {
      return true;
    }
    return false;
  }

  void CollectAtomic(Function &F, const FunIndex &index) {
    auto name = F.getName();
    if (atomic.find(name) != atomic.end()) {
      return;
    }
    atomic.insert(name);
    for (auto &B : F) {
      for (auto &I : B) {
        if (auto call = dyn_cast<CallInst>(&I)) {
          auto fun = call->getCalledFunction();
          if (fun && !fun->isDeclaration()) {
            CollectAtomic(*fun, index);
          }
        }
        if (auto invoke = dyn_cast<InvokeInst>(&I)) {
          auto fun = invoke->getCalledFunction();
          if (fun && !fun->isDeclaration()) {
            CollectAtomic(*fun, index);
          }
        }
      }
    }
  }

  void InsertYields(Function &F, const FunIndex &index) {
    auto name = F.getName();
    if (F.isDeclaration() || visited.find(name) != visited.end() ||
        atomic.find(name) != atomic.end()) {
      return;
    }

    visited.insert(name);

    Builder Builder(&*F.begin());
    for (auto &B : F) {
      for (auto it = B.begin(); std::next(it) != B.end(); ++it) {
        // !ItsYieldInst(&*std::next(it)) - after each load, store, atomicrmw
        // instruction, but not in a row
        if (NeedInterrupt(&*it, index) && !ItsYieldInst(&*std::next(it))) {
          Builder.SetInsertPoint(&*std::next(it));
          Builder.CreateCall(CoroYieldF, {})->getIterator();
          ++it;
        }
      }
    }

// This change was added to instrument methods recusively for some VK data
// structure, which had a lot of methods inside of it. This change allows to
// insert thread interleavings in the methods without marking them with
// `non_atomic` attribute. This flag is unset by default, because it might cause
// wmm tests to instrument the LTest code, which is leads to errors.
#if defined(LTEST_RECURSIVE_YIELDS)
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
#endif

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
  std::set<StringRef> atomic{};
  const std::set<const Instruction *> &skipped_accesses;
};

namespace {

struct YieldInsertPass final : public PassInfoMixin<YieldInsertPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    auto fun_index = CreateFunIndex(M);

    // Classify before adding calls, while the analyses still describe the IR.
    auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
    std::set<const Instruction *> skipped_accesses;
    for (auto &F : M) {
      if (F.isDeclaration()) {
        continue;
      }
      auto &AA = FAM.getResult<AAManager>(F);
      SmallDenseMap<const Value *, bool, 8> captured;
      for (auto &B : F) {
        for (auto &I : B) {
          auto *load = dyn_cast<LoadInst>(&I);
          auto *store = dyn_cast<StoreInst>(&I);
          if ((!load && !store) || (load && !load->isSimple()) ||
              (store && !store->isSimple())) {
            continue;
          }
          auto *pointer = load ? load->getPointerOperand()
                               : store->getPointerOperand();
          auto *object = getUnderlyingObject(pointer);
          // A noalias argument alone does not establish thread-private storage.
          if ((isa<AllocaInst>(object) &&
               isNonEscapingLocalObject(object, &captured)) ||
              (load && AA.pointsToConstantMemory(MemoryLocation::get(load),
                                                false))) {
            skipped_accesses.insert(&I);
          }
        }
      }
    }

    YieldInserter gen{M, skipped_accesses};
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
            // This parsing we need for testing with opt
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &mpm,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "mainreplacer") {
                    mpm.addPass(MainReplacerPass());
                    return true;
                  }
                  return false;
                });
            PB.registerOptimizerLastEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel Level) {
                  MPM.addPass(YieldInsertPass());
#if defined(LTEST_EXTERNAL_LAUNCH)
                  MPM.addPass(MainReplacerPass());
#endif
                });
          }};
}
