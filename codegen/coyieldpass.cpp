
#include <llvm-19/llvm/IR/BasicBlock.h>
#include <llvm-19/llvm/IR/Constant.h>
#include <llvm-19/llvm/IR/Instruction.h>
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

#include <cassert>
#include <functional>
#include <memory>
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
constexpr std::string_view create_thread = "CreateNewVirtualThread";
constexpr std::string_view wait_thread = "WaitForThread";

constexpr std::string_view co_await_ready = "await_ready";
constexpr int resumed_coro = 0;
static cl::opt<std::string> input_list(
    "coroutine-file", cl::desc("Specify path to file with config"),
    llvm::cl::Required);
;

constexpr bool dump_before = false;
constexpr bool dump_after = false;

enum HandleType { GENERIC_FUN, CORO_FUN, SPAWN_VIRT_THREAD, WAIT_VIRT_THREAD };

struct InsertPlace {
  InsertPlace() = default;
  std::optional<std::string> co_name;
  std::optional<std::string> parent_name;
  // i believe that adding column is overkill
  std::optional<unsigned int> debug_line;
  std::optional<std::string> debug_file;
};

// LLVM is built without rtti by default, so no vtables
struct InsertAction {
  InsertAction(HandleType type) : type(type) {}
  InsertPlace place;
  std::string print_name;
  HandleType type;
  virtual ~InsertAction() {}
};
struct InsertCoro : InsertAction {
  InsertCoro() : InsertAction(HandleType::CORO_FUN) {}
};

struct InsertGeneric : InsertAction {
  InsertGeneric() : InsertAction(HandleType::GENERIC_FUN) {}
};

struct InsertSpawn : InsertAction {
  int creation_id;
  InsertSpawn(int creation_id)
      : InsertAction(HandleType::SPAWN_VIRT_THREAD), creation_id(creation_id) {}
};

struct InsertWaitThread : InsertAction {
  std::vector<int> wait_for_ids;
  InsertWaitThread(const std::vector<int> &wait)
      : InsertAction(HandleType::WAIT_VIRT_THREAD), wait_for_ids(wait) {}
};

using InsertActionPtr = std::shared_ptr<InsertAction>;

namespace llvm {
namespace yaml {
static std::map<std::string_view, std::function<InsertActionPtr(IO &io)>>
    construct_action{
        {"Generic", [](IO &io) { return std::make_shared<InsertGeneric>(); }},
        {"Coro", [](IO &io) { return std::make_shared<InsertCoro>(); }},
        {"Spawn",
         [](IO &io) {
           int creation_id;
           io.mapRequired("CreationId", creation_id);
           return std::make_shared<InsertSpawn>(creation_id);
         }},
        {"Wait", [](IO &io) {
           std::vector<int> vect;
           io.mapRequired("WaitsFor", vect);
           assert(vect.size() > 0);
           return std::make_shared<InsertWaitThread>(vect);
         }}};
template <>
struct MappingTraits<InsertActionPtr> {
  static void mapping(IO &io,
                      InsertActionPtr &action) {  // NOLINT
    std::string type;
    // theoreticaly it should work with tags, but it don't work(
    io.mapRequired("Action", type);
    auto entry = construct_action.find(type);
    if (entry == construct_action.end()) {
      io.setError("Got unexpected action -  " + type);
      return;
    }
    action = (entry->second)(io);
    io.mapRequired("Name", action->print_name);
    io.mapRequired("Place", action->place);
  }
};

template <>
struct MappingTraits<InsertPlace> {
  static void mapping(IO &io, InsertPlace &cofilter) {  // NOLINT
    io.mapOptional("Function", cofilter.co_name);
    io.mapOptional("Parent", cofilter.parent_name);

    io.mapOptional("Line", cofilter.debug_line);
    io.mapOptional("File", cofilter.debug_file);
  }
};
}  // namespace yaml
}  // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(InsertActionPtr);

struct CoYieldInserter {
  Builder builder;
  CoYieldInserter(Module &m, std::vector<InsertActionPtr> &&co_filter)
      : m(m), co_filter(std::move(co_filter)), builder(m.getContext()) {
    auto &context = m.getContext();
    coro_yield_f = m.getOrInsertFunction(
        costatus_change,
        FunctionType::get(Type::getVoidTy(context),
                          {PointerType::get(Type::getInt8Ty(context), 0),
                           Type::getInt1Ty(context)},
                          {}));
    create_thread_f = m.getOrInsertFunction(
        create_thread,
        FunctionType::get(Type::getVoidTy(context),
                          {PointerType::get(Type::getInt8Ty(context), 0),
                           PointerType::get(context, 0)},
                          {}));
    wait_thread_f = m.getOrInsertFunction(
        wait_thread,
        FunctionType::get(
            Type::getVoidTy(context),
            {PointerType::get(context, 0), Type::getInt32Ty(context)}, {}));
  }

  void Run() {
    if (dump_before) {
      m.dump();
      errs().flush();
    }
    for (auto &f : m) {
      if (ignored.contains(&f)) {
        continue;
      }
      std::string demangled = demangle(f.getName());
      auto filt =
          co_filter | std::ranges::views::filter(
                          [&demangled](const InsertActionPtr &a) -> bool {
                            const auto &place = a->place;
                            return !place.parent_name ||
                                   place.parent_name == demangled;
                          });
      if (!filt.empty()) {
        InsertContextSwitchFunctions(filt, f);
      }
    }
    for (auto inst : to_delete) {
      inst->eraseFromParent();
    }
    if (dump_after) {
      m.dump();
      errs().flush();
    }
  }

 private:
  void InsertContextSwitchFunctions(auto filt, Function &f) {
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
          // filt and filt | filter have different types
          if (auto debugLoc = call->getDebugLoc()) {
            auto place_filt =
                filt |
                std::ranges::views::filter(
                    [&co_name, &debugLoc](const InsertActionPtr &a) -> bool {
                      const auto &place = a->place;
                      if (place.debug_file.has_value() &&
                          place.debug_file !=
                              debugLoc->getFile()->getFilename()) {
                        return false;
                      }
                      if (place.debug_line.has_value() &&
                          place.debug_line != debugLoc.getLine()) {
                        return false;
                      }
                      return true;
                    });
            InsertContextSwitchFun(place_filt, call_inst, invoke, co_name);

          } else {
            InsertContextSwitchFun(filt, call_inst, invoke, co_name);
          }
        }
      }
    }
  }

  void InsertContextSwitchFun(auto filt, CallInst *call, InvokeInst *invoke,
                              std::string co_name) {
    auto await_ready_ind = co_name.find(co_await_ready);
    if (await_ready_ind != std::string::npos) {
      auto res_filt =
          filt | std::ranges::views::filter(
                     [&co_name](const InsertActionPtr &a) -> bool {
                       const auto &place = a->place;
                       return !place.co_name ||
                              co_name.find(*place.co_name) != std::string::npos;
                     });
      if (!res_filt.empty()) {
        auto entry = res_filt.front();
        switch (entry->type) {
          case HandleType::CORO_FUN: {
            errs() << "inserted coro handled by type " << co_name << "\n";
            auto start = [this, &entry]() { InsertYieldCall(entry, true); };
            auto end = [this, &entry]() { InsertYieldCall(entry, false); };
            HandleCoroCase(call, res_filt.front(), start, end);
            return;
          }
          default:
            return;
        }
      }
      return;
    }
    auto res_filt =
        filt | std::ranges::views::filter(
                   [&co_name](const InsertActionPtr &a) -> bool {
                     const auto &place = a->place;
                     return !place.co_name || place.co_name == co_name;
                   });

    if (!res_filt.empty()) {
      auto entry = res_filt.front();
      switch (entry->type) {
        case HandleType::CORO_FUN: {
          errs() << "inserted coro handled by func name " << co_name << "\n";
          auto start = [this, &entry]() { InsertYieldCall(entry, true); };
          auto end = [this, &entry]() { InsertYieldCall(entry, false); };
          if (invoke) {
            assert(FindAwaitReady(invoke->getNormalDest()->begin(), entry,
                                  start, end));
          } else {
            assert(FindAwaitReady(BasicBlock::iterator(call->getNextNode()),
                                  entry, start, end));
          }
          break;
        }
        case HandleType::GENERIC_FUN: {
          errs() << "inserted generic " << co_name << "\n";
          HandleGenericFunCase(call, invoke, entry);
          break;
        }
        case HandleType::SPAWN_VIRT_THREAD: {
          errs() << "inserted spawn of new thread " << co_name << "\n";
          CallBase *inst = call ? static_cast<CallBase *>(call) : invoke;
          Function *called_fun = inst->getCalledFunction();

          if (!inst->arg_empty()) {
            auto [wrapper_fun, storage] = InsertZeroArgsWrapper(inst);
            builder.SetInsertPoint(inst);
            for (size_t i = 0; i < called_fun->arg_size(); i++) {
              Value *arg = inst->getArgOperand(i);

              Value *storage_place =
                  builder.CreateGEP(storage->getValueType(), storage,
                                    {
                                        builder.getInt32(0),
                                        builder.getInt32(i),
                                    });
              builder.CreateStore(arg, storage_place);
            }
            called_fun = wrapper_fun;
          }
          Value *pointer_to_func = builder.CreatePointerCast(
              called_fun, PointerType::get(builder.getContext(), 0));
          std::shared_ptr<InsertSpawn> spawn =
              std::static_pointer_cast<InsertSpawn>(entry);
          Value *id = builder.getInt32(spawn->creation_id);
          builder.SetInsertPoint(inst);
          Value *replacement;
          if (call) {
            replacement = builder.CreateCall(
                create_thread_f,
                {id, GetLiteral(entry->print_name), pointer_to_func});
          } else {
            replacement = builder.CreateInvoke(
                create_thread_f, invoke->getNormalDest(),
                invoke->getUnwindDest(),
                {id, GetLiteral(entry->print_name), pointer_to_func});
          }
          inst->replaceAllUsesWith(replacement);
          // we cannot simple delete here instruction because we are
          // iterating over it in basic block
          to_delete.push_back(inst);
          break;
        }
        case HandleType::WAIT_VIRT_THREAD: {
          errs() << "inserted wait of new thread" << co_name << "\n";
          // I'm sure that here must be only coro case
          auto start = std::bind_front(
              &CoYieldInserter::InsertWaitFunc, this,
              std::static_pointer_cast<InsertWaitThread>(entry));
          if (invoke) {
            assert(FindAwaitReady(invoke->getNormalDest()->begin(), entry,
                                  start, {}));
          } else {
            assert(FindAwaitReady(BasicBlock::iterator(call->getNextNode()),
                                  entry, start, {}));
          }
          break;
        }
        default:
          __builtin_unreachable();
      }
    }
  }
  void InsertWaitFunc(const std::shared_ptr<InsertWaitThread> &act) {
    // because in c++ this function will have pointer as argument, we need also
    // explicitly pass the size
    std::vector<Constant *> elements;
    for (auto &a : act->wait_for_ids) {
      elements.push_back(builder.getInt32(a));
    }
    Constant *indices = ConstantArray::get(
        ArrayType::get(builder.getInt32Ty(), act->wait_for_ids.size()),
        elements);
    GlobalVariable *val_ind = new GlobalVariable(m,
        indices->getType(), true, llvm::GlobalValue::InternalLinkage, indices);
    builder.CreateCall(wait_thread_f,
                       {val_ind, builder.getInt32(elements.size())});
  }

  // We need pass to scheduler function and wan't to care about number
  // of args and their type to not interact with templates - so lets create a
  // wrapper which would have zero args
  std::pair<Function *, GlobalVariable *> InsertZeroArgsWrapper(
      CallBase *call_inst) {
    Function *func =
        Function::Create(FunctionType::get(Type::getVoidTy(m.getContext()), {}),
                         GlobalValue::PrivateLinkage, "", m);
    ignored.insert(func);
    std::vector<Type *> types;
    for (auto &arg : call_inst->args()) {
      types.push_back(arg->getType());
    }
    StructType *storage_type = StructType::create(types);
    GlobalVariable *storage =
        new GlobalVariable(m, storage_type, false, GlobalValue::PrivateLinkage,
                           Constant::getNullValue(storage_type));
    BasicBlock *block = BasicBlock::Create(builder.getContext(), "", func);
    builder.SetInsertPoint(block);
    std::vector<Value *> args;
    for (size_t i = 0; i < types.size(); i++) {
      Value *load = builder.CreateGEP(
          storage_type, storage, {builder.getInt32(0), builder.getInt32(i)});
      args.push_back(builder.CreateLoad(types[i], load));
    }
    builder.CreateCall(call_inst->getCalledFunction(), {args});
    builder.CreateRetVoid();
    return {func, storage};
  }

  bool FindAwaitReady(BasicBlock::iterator start, InsertActionPtr &entry,
                      const std::function<void()> &start_insert,
                      const std::function<void()> &end_insert) {
    for (Instruction &n_inst : make_range(start, start->getParent()->end())) {
      auto *call_inst = dyn_cast<CallBase>(&n_inst);
      if (!call_inst) {
        continue;
      }
      auto await_ready_ind = demangle(call_inst->getCalledFunction()->getName())
                                 .find(co_await_ready);
      if (await_ready_ind != std::string::npos) {
        HandleCoroCase(call_inst, entry, start_insert, end_insert);
        return true;
      }
      // If Coro Type constructor can throw we need go deeper
      if (auto *invoke = dyn_cast<InvokeInst>(call_inst)) {
        return FindAwaitReady(invoke->getNormalDest()->begin(), entry,
                              start_insert, end_insert);
      }
    }
    return false;
  }
  // This case is needed at sample by some coro primitives where the
  // normal function which is the body of coro is called in loop
  void HandleGenericFunCase(CallBase *call, InvokeInst *invoke,
                            const InsertActionPtr &filt_entry) {
    builder.SetInsertPoint(call);
    InsertYieldCall(filt_entry, true);
    // Invoke instruction has unwind/normal ends so we need handle it
    if (invoke) {
      builder.SetInsertPoint(invoke->getNormalDest()->getFirstInsertionPt());
      InsertYieldCall(filt_entry, false);
      builder.SetInsertPoint(invoke->getUnwindDest()->getFirstInsertionPt());
      InsertYieldCall(filt_entry, false);
    } else {
      builder.SetInsertPoint(call->getNextNode());
      InsertYieldCall(filt_entry, false);
    }
  }

  void HandleCoroCase(CallBase *call, const InsertActionPtr &filt_entry,
                      const std::function<void()> &start_insert,
                      const std::function<void()> &end_insert) {
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
        // We cannot insert after await_suspend because inside it we can
        // already interact with handle, so we must we do it before
        case Intrinsic::coro_await_suspend_bool: {
          if (start_insert) {
            builder.SetInsertPoint(call_base);
            start_insert();
          }
          if (end_insert) {
            BranchInst *suspend_br = dyn_cast<BranchInst>(i.getNextNode());
            assert(suspend_br && suspend_br->getNumSuccessors() == 2);
            builder.SetInsertPoint(
                suspend_br->getSuccessor(0)->getFirstInsertionPt());
            // handled if await_suspend was true, now change block also for
            // false
            BasicBlock *tramp = InsertAtEnd(
                builder, &(*builder.GetInsertPoint()), filt_entry, end_insert);
            suspend_br->setSuccessor(1, tramp);
          }
          return;
        }
        case Intrinsic::coro_await_suspend_void: {
          if (start_insert) {
            builder.SetInsertPoint(call_base);
            start_insert();
          }
          if (end_insert) {
            InsertAtEnd(builder, nextNormal(call_base), filt_entry, end_insert);
          }
          return;
        }
        case Intrinsic::coro_await_suspend_handle: {
          if (start_insert) {
            builder.SetInsertPoint(call_base);
            start_insert();
          }
          if (end_insert) {
            InsertAtEnd(builder, nextNormal(call_base), filt_entry, end_insert);
          }
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
                          const InsertActionPtr &filt_entry,
                          const std::function<void()> &end_insert) {
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
    end_insert();
    builder.CreateBr(succ);
    return tramp;
  }
  void InsertYieldCall(const InsertActionPtr &filt, bool start) {
    auto llvm_start =
        ConstantInt::get(Type::getInt1Ty(builder.getContext()), start);
    builder.CreateCall(coro_yield_f,
                       {GetLiteral(filt->print_name), llvm_start});
  }
  Constant *GetLiteral(const std::string &name) {
    auto literal = string_literals.find(name);
    if (literal == string_literals.end()) {
      Constant *str_const =
          ConstantDataArray::getString(m.getContext(), name, true);
      auto zero = ConstantInt::get(Type::getInt32Ty(m.getContext()), 0);
      std::array<Constant *, 2> ind = {zero, zero};
      GlobalVariable *global =
          new GlobalVariable(m, str_const->getType(), true,
                             GlobalValue::PrivateLinkage, str_const);
      auto ptr =
          ConstantExpr::getGetElementPtr(global->getValueType(), global, ind);
      literal = string_literals.emplace(name, ptr).first;
    }
    return literal->second;
  }
  Module &m;
  FunctionCallee coro_yield_f;
  FunctionCallee create_thread_f;
  FunctionCallee wait_thread_f;
  std::vector<InsertActionPtr> co_filter;
  std::map<std::string, Constant *> string_literals;
  std::vector<Instruction *> to_delete;
  std::set<Function *> ignored;
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
    std::vector<InsertActionPtr> filt;
    input >> filt;

    if (input.error()) {
      errs() << "Tried to parse file " << input_list << "\n";
      report_fatal_error("Error parsing YAML\n", false);
    }
    CoYieldInserter gen{m, std::move(filt)};
    gen.Run();
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
                  // Looks like we don't need any lowerings, before but i'm
                  // not sure
                  //  mpm.addPass(CoroEarlyPass());
                  mpm.addPass(CoYieldInsertPass());
                });
          }};
}