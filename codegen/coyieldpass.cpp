
#include <llvm-19/llvm/IR/BasicBlock.h>
#include <llvm-19/llvm/IR/Constant.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/ObjectYAML/YAML.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Coroutines/CoroEarly.h>

#include <algorithm>
#include <cassert>
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
#include "llvm/Transforms/Coroutines/CoroSplit.h"
#include "llvm/Transforms/Utils/Cloning.h"
using namespace llvm;
using Builder = IRBuilder<>;

constexpr std::string_view costatus_change = "CoroutineStatusChange";
constexpr std::string_view co_await_ready = "await_ready";
constexpr int resumed_coro = 0;
static cl::opt<std::string> input_list(
    "coroutine-file", cl::desc("Specify path to file with coroutines to check"),
    llvm::cl::Required);
;
constexpr bool dump_before = false;
constexpr bool dump_after = true;

struct CoroutineFilter {
  CoroutineFilter() = default;
  CoroutineFilter(const std::optional<std::string> &parent_name,
                  const std::optional<std::string> &co_name,
                  const std::string &print_name)
      : parent_name(parent_name), co_name(co_name), print_name(print_name) {};
  std::optional<std::string> parent_name;
  std::optional<std::string> co_name;
  std::string print_name;
  bool only_fun;
};

namespace llvm {
namespace yaml {
template <>
struct MappingTraits<CoroutineFilter> {
  static void mapping(IO &io, CoroutineFilter &cofilter) {  // NOLINT
    io.mapRequired("Name", cofilter.print_name);
    io.mapOptional("Coroutine", cofilter.co_name);
    io.mapOptional("Parent", cofilter.parent_name);
    io.mapRequired("OnlyFun", cofilter.only_fun);
  }
};
}  // namespace yaml
}  // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(CoroutineFilter);

struct CoYieldInserter {
  CoYieldInserter(Module &m, std::vector<CoroutineFilter> &&co_filter)
      : m(m), co_filter(std::move(co_filter)) {
    auto &context = m.getContext();
    coroYieldF = m.getOrInsertFunction(
        costatus_change,
        FunctionType::get(Type::getVoidTy(context),
                          {PointerType::get(Type::getInt8Ty(context), 0),
                           Type::getInt1Ty(context)},
                          {}));
  }

  void Run(const Module &index) {
    if (dump_before) {
      index.dump();
      errs().flush();
    }
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
    if (dump_after) {
      index.dump();
      errs().flush();
    }
  }

 private:
  void InsertYields(auto filt, Function &f) {
    Builder builder(&*f.begin());
    for (auto &b : f) {
      for (auto &i : b) {
        CallBase *call = dyn_cast<CallBase>(&i);
        if (!call) {
          continue;
        }
        auto c_fn = call->getCalledFunction();
        if (c_fn == nullptr) {
          continue;
        }
        auto raw_fn_name = c_fn->getName();
        std::string co_name = demangle(raw_fn_name);
        if (co_name == costatus_change) {
          continue;
        }
        CallInst *call_inst = dyn_cast<CallInst>(call);
        InvokeInst *invoke = dyn_cast<InvokeInst>(call);

        if (call_inst || invoke) {
          auto await_ready_ind = co_name.find(co_await_ready);
          if (await_ready_ind != std::string::npos) {
            auto res_filt =
                filt | std::ranges::views::filter(
                           [&co_name](const CoroutineFilter &a) -> bool {
                             return !a.co_name || co_name.find(*a.co_name) !=
                                                      std::string::npos;
                           });
            if (!res_filt.empty()) {
              errs() << "inserted coro handled by type " << co_name << "\n";
              HandleCoroCase(builder, call, res_filt.front());
            }
          } else {
            auto res_filt =
                filt | std::ranges::views::filter(
                           [&co_name](const CoroutineFilter &a) -> bool {
                             return !a.co_name || a.co_name == co_name;
                           });

            if (!res_filt.empty()) {
              auto entry = res_filt.front();
              if (entry.only_fun) {
                errs() << "inserted generic" << co_name << "\n";
                HandleGenericFunCase(builder, call, invoke, entry);
              } else {
                errs() << "inserted coro handled by func name" << co_name
                       << "\n";
                if (invoke) {
                  assert(FindAwaitReady(
                      builder, invoke->getNormalDest()->begin(), entry));
                } else {
                  assert(FindAwaitReady(
                      builder, BasicBlock::iterator(call->getNextNode()),
                      entry));
                }
              }
            }
          }
        }
      }
    }
  }

  bool FindAwaitReady(Builder &builder, BasicBlock::iterator start,
                      const CoroutineFilter &entry) {
    for (Instruction &n_inst : make_range(start, start->getParent()->end())) {
      auto *call_inst = dyn_cast<CallBase>(&n_inst);
      if (!call_inst) {
        continue;
      }
      auto await_ready_ind = demangle(call_inst->getCalledFunction()->getName())
                                 .find(co_await_ready);
      if (await_ready_ind != std::string::npos) {
        HandleCoroCase(builder, call_inst, entry);
        return true;
      }
      // If Coro Type constructor can throw we need go deeper
      if (auto *invoke = dyn_cast<InvokeInst>(call_inst)) {
        return FindAwaitReady(builder, invoke->getNormalDest()->begin(), entry);
      }
    }
    return false;
  }
  // This case is needed at sample by some coro primitives where the
  // normal function which is the body of coro is called in loop
  void HandleGenericFunCase(Builder &builder, CallBase *call,
                            InvokeInst *invoke,
                            const CoroutineFilter &filt_entry) {
    builder.SetInsertPoint(call);
    InsertCall(filt_entry, builder, true);
    // Invoke instruction has unwind/normal ends so we need handle it
    if (invoke) {
      builder.SetInsertPoint(invoke->getNormalDest()->getFirstInsertionPt());
      InsertCall(filt_entry, builder, false);
      builder.SetInsertPoint(invoke->getUnwindDest()->getFirstInsertionPt());
      InsertCall(filt_entry, builder, false);
    } else {
      builder.SetInsertPoint(call->getNextNode());
      InsertCall(filt_entry, builder, false);
    }
  }

  void HandleCoroCase(Builder &builder, CallBase *call,
                      const CoroutineFilter &filt_entry) {
    BranchInst *br = dyn_cast<BranchInst>(call->getNextNode());
    assert(br && br->getNumSuccessors() == 2);
    BasicBlock *not_ready_bb = br->getSuccessor(1);
    for (auto &i : *not_ready_bb) {
      CallBase *call_base = dyn_cast<CallBase>(&i);
      if (!call_base) {
        continue;
      }

      Intrinsic::ID id = call_base->getIntrinsicID();
      switch (id) {
        // We cannot insert after await_suspend because inside it we can already
        // interact with handle, so we must we do it before
        case Intrinsic::coro_await_suspend_bool: {
          builder.SetInsertPoint(call_base);
          InsertCall(filt_entry, builder, true);
          // InsertAtEnd(builder, nextNormal(call_base), filt_entry);
          BranchInst *suspend_br = dyn_cast<BranchInst>(i.getNextNode());
          assert(suspend_br && suspend_br->getNumSuccessors() == 2);
          builder.SetInsertPoint(
              suspend_br->getSuccessor(0)->getFirstInsertionPt());
          // InsertCall(filt_entry, builder, true);
          // handled if await_suspend was true, now change block also for false
          BasicBlock *tramp =
              InsertAtEnd(builder, &(*builder.GetInsertPoint()), filt_entry);
          suspend_br->setSuccessor(1, tramp);
          return;
        }
        case Intrinsic::coro_await_suspend_void: {
          builder.SetInsertPoint(call_base);
          InsertCall(filt_entry, builder, true);
          InsertAtEnd(builder, call_base++, filt_entry);
          return;
        }
        case Intrinsic::coro_await_suspend_handle: {
          builder.SetInsertPoint(call_base);
          InsertCall(filt_entry, builder, true);
          InsertAtEnd(builder, nextNormal(call_base), filt_entry);
          return;
        }
        default: {
          continue;
        }
      }
    }
    assert(false && "Haven't found await_suspend intrisinc");
  }
  Instruction *nextNormal(CallBase *inst) {
    if (auto invoke = dyn_cast<InvokeInst>(inst)) {
      return invoke->getNormalDest()->getFirstNonPHI();
    } else {
      return inst->getNextNode();
    }
  }
  BasicBlock *InsertAtEnd(Builder &builder, Instruction *instr,
                          const CoroutineFilter &filt_entry) {
    CallInst *intr = dyn_cast<CallInst>(instr);
    assert(intr && intr->getIntrinsicID() == Intrinsic::coro_suspend);
    SwitchInst *switch_inst = dyn_cast<SwitchInst>(intr->getNextNode());
    assert(switch_inst);
    auto resumed_bb = switch_inst->findCaseValue(
        ConstantInt::get(Type::getInt8Ty(builder.getContext()), resumed_coro));
    auto succ = resumed_bb->getCaseSuccessor();
    // If we would simple insert in the block we would have extra ends, so we
    // need to add a trampoline
    BasicBlock *tramp =
        BasicBlock::Create(builder.getContext(), "", succ->getParent());
    resumed_bb->setSuccessor(tramp);
    builder.SetInsertPoint(tramp);
    InsertCall(filt_entry, builder, false);
    builder.CreateBr(succ);
    return tramp;
  }
  void InsertCall(const CoroutineFilter &filt, Builder &builder, bool start) {
    auto llvm_start =
        ConstantInt::get(Type::getInt1Ty(builder.getContext()), start);
    auto literal = string_literals.find(filt.print_name);
    if (literal == string_literals.end()) {
      Constant *str_const =
          ConstantDataArray::getString(m.getContext(), filt.print_name, true);
      auto zero = ConstantInt::get(Type::getInt32Ty(m.getContext()), 0);
      std::array<Constant *, 2> ind = {zero, zero};
      GlobalVariable *global =
          new GlobalVariable(m, str_const->getType(), true,
                             GlobalValue::PrivateLinkage, str_const);
      auto ptr =
          ConstantExpr::getGetElementPtr(global->getValueType(), global, ind);
      literal = string_literals.emplace(filt.print_name, ptr).first;
    }
    builder.CreateCall(coroYieldF, {literal->second, llvm_start});
  }

  Module &m;
  FunctionCallee coroYieldF;
  std::vector<CoroutineFilter> co_filter;
  std::map<std::string, Constant *> string_literals;
};

namespace {

struct CoYieldInsertPass final : public PassInfoMixin<CoYieldInsertPass> {
  PreservedAnalyses run(Module &m, ModuleAnalysisManager &am) {  // NOLINT
    if (input_list.empty()) {
      report_fatal_error("No file  with coroutines list", false);
    }

    auto file = llvm::MemoryBuffer::getFile(input_list);
    if (!file) {
      errs() << "Tried to read file " << input_list << "\n";
      report_fatal_error("Failed to load config file \n", false);
    }

    llvm::yaml::Input input(file.get()->getBuffer());
    std::vector<CoroutineFilter> filt;
    input >> filt;

    if (input.error()) {
      errs() << "Tried to parse file " << input_list << "\n";
      report_fatal_error("Error parsing YAML\n", false);
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
            // This parsing we need for testing with opt
            pb.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &mpm,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "coyield_insert") {
                    mpm.addPass(CoYieldInsertPass());
                    return true;
                  }
                  return false;
                });
            pb.registerPipelineStartEPCallback(
                [](ModulePassManager &mpm, OptimizationLevel level) {
                  // Looks like we don't need any lowerings, but i'm not
                  // sure
                  //  mpm.addPass(CoroEarlyPass());
                  mpm.addPass(CoYieldInsertPass());
                });
          }};
}