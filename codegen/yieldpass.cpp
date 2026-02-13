#include <atomic>
#include <set>
#include <string>

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

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
  YieldInserter(Module &M) : M(M) {
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
    return HasAttribute(index, fun_name, nonatomic_attr);
  }

  bool IsAtomic(const StringRef fun_name, const FunIndex &index) {
    return HasAttribute(index, fun_name, atomic_attr);
  }

  bool NeedInterrupt(Instruction *insn, const FunIndex &index) {
    if (isa<LoadInst>(insn) || isa<StoreInst>(insn) ||
         isa<AtomicRMWInst>(insn) /*||
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
    if (visited.find(name) != visited.end() ||
        atomic.find(name) != atomic.end()) {
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

    errs() << "yields inserted to the " << F.getName() << "\n";
    errs() << F << "\n";
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
};

static int StdOrderFromLlvm(AtomicOrdering ordering) {
  switch (ordering) {
    case AtomicOrdering::Monotonic:
      return static_cast<int>(std::memory_order_relaxed);
    case AtomicOrdering::Acquire:
      return static_cast<int>(std::memory_order_acquire);
    case AtomicOrdering::Release:
      return static_cast<int>(std::memory_order_release);
    case AtomicOrdering::AcquireRelease:
      return static_cast<int>(std::memory_order_acq_rel);
    case AtomicOrdering::SequentiallyConsistent:
      return static_cast<int>(std::memory_order_seq_cst);
    case AtomicOrdering::NotAtomic:
    case AtomicOrdering::Unordered:
    default:
      return static_cast<int>(std::memory_order_relaxed);
  }
}

static std::string TypeSuffix(Type *ty) {
  if (!ty->isIntegerTy()) {
    return "";
  }
  unsigned bits = ty->getIntegerBitWidth();
  switch (bits) {
    case 1:
      return "i1";
    case 8:
      return "i8";
    case 16:
      return "i16";
    case 32:
      return "i32";
    case 64:
      return "i64";
    default:
      return "";
  }
}

static bool IsSupportedType(Type *ty) {
  return !TypeSuffix(ty).empty();
}

static FunctionCallee GetChoiceCallee(Module &M) {
  auto &ctx = M.getContext();
  auto i32 = Type::getInt32Ty(ctx);
  auto fty = FunctionType::get(i32, {i32}, false);
  return M.getOrInsertFunction("__ltest_wmm_choice", fty);
}

static FunctionCallee GetLoadCandidatesCallee(Module &M, Type *val_ty) {
  auto &ctx = M.getContext();
  auto suffix = TypeSuffix(val_ty);
  std::string name = "__ltest_wmm_load_candidates_" + suffix;
  auto void_ptr = PointerType::get(ctx, 0);
  auto order_ty = Type::getInt32Ty(ctx);
  std::vector<Type *> args = {void_ptr, order_ty};
  auto fty = FunctionType::get(Type::getInt32Ty(ctx), args, false);
  return M.getOrInsertFunction(name, fty);
}

static FunctionCallee GetLoadApplyCallee(Module &M, Type *val_ty) {
  auto &ctx = M.getContext();
  auto suffix = TypeSuffix(val_ty);
  std::string name = "__ltest_wmm_load_apply_" + suffix;
  auto i32 = Type::getInt32Ty(ctx);
  auto fty = FunctionType::get(val_ty, {i32}, false);
  return M.getOrInsertFunction(name, fty);
}

static FunctionCallee GetStoreCallee(Module &M, Type *val_ty) {
  auto &ctx = M.getContext();
  auto suffix = TypeSuffix(val_ty);
  std::string name = "__ltest_wmm_store_" + suffix;
  auto void_ptr = PointerType::get(ctx, 0);
  auto order_ty = Type::getInt32Ty(ctx);
  std::vector<Type *> args = {void_ptr, order_ty, val_ty};
  auto fty = FunctionType::get(Type::getVoidTy(ctx), args, false);
  return M.getOrInsertFunction(name, fty);
}

static FunctionCallee GetCmpXchgCallee(Module &M, Type *val_ty) {
  auto &ctx = M.getContext();
  auto suffix = TypeSuffix(val_ty);
  std::string name = "__ltest_wmm_cmpxchg_" + suffix;
  auto void_ptr = PointerType::get(ctx, 0);
  auto order_ty = Type::getInt32Ty(ctx);
  auto i1 = Type::getInt1Ty(ctx);
  std::vector<Type *> args = {void_ptr, val_ty, val_ty, order_ty, order_ty, i1};
  auto ret_ty = StructType::get(ctx, {val_ty, i1});
  auto fty = FunctionType::get(ret_ty, args, false);
  return M.getOrInsertFunction(name, fty);
}

static Value *ToVoidPtr(IRBuilder<> &B, Value *ptr) {
  auto *i8ptr = PointerType::get(B.getContext(), 0);
  if (ptr->getType() == i8ptr) {
    return ptr;
  }
  return B.CreateBitCast(ptr, i8ptr);
}

static bool IsStdAtomicCall(Function *F) {
  if (!F) {
    return false;
  }
  auto name = F->getName();
  if (name.starts_with("__ltest_wmm_")) {
    return false;
  }
  return name.contains("__atomic_base") || name.contains("atomic") ||
         name.contains("atomic_base");
}

static bool IsStdAtomicLoad(Function *F) {
  if (!IsStdAtomicCall(F)) {
    return false;
  }
  auto name = F->getName();
  return name.contains("load");
}

static bool IsStdAtomicStore(Function *F) {
  if (!IsStdAtomicCall(F)) {
    return false;
  }
  auto name = F->getName();
  return name.contains("store");
}

static bool IsStdAtomicCmpXchg(Function *F) {
  if (!IsStdAtomicCall(F)) {
    return false;
  }
  auto name = F->getName();
  return name.contains("compare_exchange_strong") ||
         name.contains("compare_exchange_weak");
}

static bool IsStdAtomicCmpXchgWeak(Function *F) {
  if (!IsStdAtomicCmpXchg(F)) {
    return false;
  }
  auto name = F->getName();
  return name.contains("compare_exchange_weak");
}

struct WmmAtomicInserter {
  explicit WmmAtomicInserter(Module &M) : M(M) {}

  bool Run(const FunIndex &index) {
    bool changed = false;
    for (auto &F : M) {
      if (!IsTarget(F.getName(), index)) {
        continue;
      }
      changed |= InstrumentFunction(F);
    }
    return changed;
  }

 private:
  bool IsTarget(const StringRef fun_name, const FunIndex &index) {
    return HasAttribute(index, fun_name, nonatomic_attr);
  }

  bool InstrumentFunction(Function &F) {
    SmallVector<Instruction *, 64> work;
    for (auto &B : F) {
      for (auto &I : B) {
        if (auto *LI = dyn_cast<LoadInst>(&I)) {
          if (LI->isAtomic()) {
            work.push_back(LI);
          }
        } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
          if (SI->isAtomic()) {
            work.push_back(SI);
          }
        } else if (auto *CX = dyn_cast<AtomicCmpXchgInst>(&I)) {
          work.push_back(CX);
        } else if (auto *RMW = dyn_cast<AtomicRMWInst>(&I)) {
          work.push_back(RMW);
        } else if (auto *CI = dyn_cast<CallInst>(&I)) {
          if (auto *CF = CI->getCalledFunction()) {
            if (IsStdAtomicLoad(CF) || IsStdAtomicStore(CF) ||
                IsStdAtomicCmpXchg(CF)) {
              work.push_back(CI);
            }
          }
        }
      }
    }

    bool changed = false;
    for (auto *I : work) {
      if (auto *LI = dyn_cast<LoadInst>(I)) {
        changed |= InstrumentLoad(LI);
      } else if (auto *SI = dyn_cast<StoreInst>(I)) {
        changed |= InstrumentStore(SI);
      } else if (auto *CX = dyn_cast<AtomicCmpXchgInst>(I)) {
        changed |= InstrumentCmpXchg(CX);
      } else if (auto *RMW = dyn_cast<AtomicRMWInst>(I)) {
        changed |= InstrumentRmw(RMW);
      } else if (auto *CI = dyn_cast<CallInst>(I)) {
        changed |= InstrumentStdAtomicCall(CI);
      }
    }

    return changed;
  }

  bool InstrumentLoad(LoadInst *LI) {
    auto *val_ty = LI->getType();
    if (!IsSupportedType(val_ty)) {
      return false;
    }

    IRBuilder<> B(LI);
    auto *order = B.getInt32(StdOrderFromLlvm(LI->getOrdering()));
    auto *addr = ToVoidPtr(B, LI->getPointerOperand());
    auto load_candidates = GetLoadCandidatesCallee(M, val_ty);
    auto *count = B.CreateCall(load_candidates, {addr, order});
    auto choice_callee = GetChoiceCallee(M);
    auto *choice = B.CreateCall(choice_callee, {count});
    auto load_apply = GetLoadApplyCallee(M, val_ty);
    auto *call = B.CreateCall(load_apply, {choice});

    LI->replaceAllUsesWith(call);
    LI->eraseFromParent();
    return true;
  }

  bool InstrumentStore(StoreInst *SI) {
    auto *val_ty = SI->getValueOperand()->getType();
    if (!IsSupportedType(val_ty)) {
      return false;
    }

    IRBuilder<> B(SI);
    auto *order = B.getInt32(StdOrderFromLlvm(SI->getOrdering()));
    auto *addr = ToVoidPtr(B, SI->getPointerOperand());
    auto *value = SI->getValueOperand();
    auto callee = GetStoreCallee(M, val_ty);
    B.CreateCall(callee, {addr, order, value});
    SI->eraseFromParent();
    return true;
  }

  bool InstrumentCmpXchg(AtomicCmpXchgInst *CX) {
    auto *val_ty = CX->getCompareOperand()->getType();
    if (!IsSupportedType(val_ty)) {
      return false;
    }

    IRBuilder<> B(CX);
    auto *order_success = B.getInt32(StdOrderFromLlvm(CX->getSuccessOrdering()));
    auto *order_failure = B.getInt32(StdOrderFromLlvm(CX->getFailureOrdering()));
    auto *addr = ToVoidPtr(B, CX->getPointerOperand());
    auto *expected = CX->getCompareOperand();
    auto *desired = CX->getNewValOperand();
    auto *is_weak = B.getInt1(CX->isWeak());

    auto callee = GetCmpXchgCallee(M, val_ty);
    auto *call = B.CreateCall(
        callee, {addr, expected, desired, order_success, order_failure, is_weak});

    auto *new_old = ExtractValueInst::Create(call, {0}, "", CX->getNextNode());
    auto *new_success = ExtractValueInst::Create(call, {1}, "", CX->getNextNode());

    SmallVector<Instruction *, 8> to_remove;
    for (auto *user : CX->users()) {
      if (auto *EV = dyn_cast<ExtractValueInst>(user)) {
        if (EV->getIndices()[0] == 0) {
          EV->replaceAllUsesWith(new_old);
          to_remove.push_back(EV);
        } else if (EV->getIndices()[0] == 1) {
          EV->replaceAllUsesWith(new_success);
          to_remove.push_back(EV);
        }
      }
    }

    for (auto *I : to_remove) {
      I->eraseFromParent();
    }

    if (CX->use_empty()) {
      CX->eraseFromParent();
    }

    return true;
  }

  bool InstrumentRmw(AtomicRMWInst *RMW) {
    // rmw is not fully supported by wmm graph
    (void)RMW;
    return false;
  }

  bool InstrumentStdAtomicCall(CallInst *CI) {
    auto *CF = CI->getCalledFunction();
    if (!CF) {
      return false;
    }

    if (IsStdAtomicLoad(CF)) {
        if (CI->arg_size() < 1) {
          return false;
        }
      auto *addr = CI->getArgOperand(0);
        Value *order = nullptr;
        if (CI->arg_size() >= 2) {
          order = CI->getArgOperand(1);
        }
      auto *val_ty = CI->getType();
      if (!IsSupportedType(val_ty)) {
        return false;
      }
      IRBuilder<> B(CI);
        if (!order) {
          // default std::atomic load uses seq_cst.
          order = B.getInt32(static_cast<int>(std::memory_order_seq_cst));
        }
      auto load_candidates = GetLoadCandidatesCallee(M, val_ty);
      auto *count = B.CreateCall(load_candidates, {ToVoidPtr(B, addr), order});
      auto choice_callee = GetChoiceCallee(M);
      auto *choice = B.CreateCall(choice_callee, {count});
      auto load_apply = GetLoadApplyCallee(M, val_ty);
      auto *call = B.CreateCall(load_apply, {choice});
      CI->replaceAllUsesWith(call);
      CI->eraseFromParent();
      return true;
    }

    if (IsStdAtomicStore(CF)) {
      if (CI->arg_size() < 2) {
        return false;
      }
      auto *addr = CI->getArgOperand(0);
      auto *value = CI->getArgOperand(1);
      Value *order = nullptr;
      if (CI->arg_size() >= 3) {
        order = CI->getArgOperand(2);
      }
      auto *val_ty = value->getType();
      if (!IsSupportedType(val_ty)) {
        return false;
      }
      IRBuilder<> B(CI);
      if (!order) {
        // default std::atomic store uses seq_cst
        order = B.getInt32(static_cast<int>(std::memory_order_seq_cst));
      }
      auto callee = GetStoreCallee(M, val_ty);
      B.CreateCall(callee, {ToVoidPtr(B, addr), order, value});
      CI->eraseFromParent();
      return true;
    }

    if (IsStdAtomicCmpXchg(CF)) {
      if (CI->arg_size() < 4) {
        return false;
      }
      auto *addr = CI->getArgOperand(0);
      auto *expected_ptr = CI->getArgOperand(1);
      auto *desired = CI->getArgOperand(2);
      auto *order_success = CI->getArgOperand(3);
      Value *order_failure = nullptr;
      if (CI->arg_size() >= 5) {
        order_failure = CI->getArgOperand(4);
      }
      auto *val_ty = desired->getType();
      if (!IsSupportedType(val_ty)) {
        return false;
      }

      IRBuilder<> B(CI);
      auto *expected_val = B.CreateLoad(val_ty, expected_ptr);
      auto *is_weak = B.getInt1(IsStdAtomicCmpXchgWeak(CF));
      if (!order_failure) {
        // overload with single memory_order
        order_failure = order_success;
      }
      auto callee = GetCmpXchgCallee(M, val_ty);
      auto *call = B.CreateCall(
          callee,
          {ToVoidPtr(B, addr), expected_val, desired, order_success,
           order_failure,
           is_weak});

      Instruction *insert_pt = CI->getNextNode();
      if (!insert_pt) {
        insert_pt = CI->getParent()->getTerminator();
      }
      IRBuilder<> Bafter(insert_pt);
      auto *new_old = Bafter.CreateExtractValue(call, {0});
      auto *new_success = Bafter.CreateExtractValue(call, {1});
      Bafter.CreateStore(new_old, expected_ptr);

      CI->replaceAllUsesWith(new_success);
      CI->eraseFromParent();
      return true;
    }

    return false;
  }

  Module &M;
};

namespace {

struct YieldInsertPass final : public PassInfoMixin<YieldInsertPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    auto fun_index = CreateFunIndex(M);

    WmmAtomicInserter wmm{M};
    wmm.Run(fun_index);

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