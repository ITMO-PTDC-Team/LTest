#include <set>
#include <utility>
#include <vector>

#include "llvm/Demangle/Demangle.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "utils.h"

using namespace llvm;

using builder_t = IRBuilder<NoFolder>;
using fun_index_t = std::set<std::pair<std::string, StringRef>>;

const std::string coro_suf = "_coro";
const std::string task_builder_suf = "_task_builder";
const std::string gen_annotation = "generator";
const std::string nonatomic_annotation = "nonatomic";

template <typename T>
void Assert(bool cond, const T &obj) {
  if (!cond) {
    errs() << "Assertion failed: \n";
    errs() << obj << "\n";
    assert(cond);
  }
}

void Assert(bool cond) { assert(cond); }

Value *GenerateCall(builder_t *builder, Function *fun) {
  if (fun->arg_size() == 0) {
    return builder->CreateCall(fun, {});
  }
  Assert(fun->arg_size() == 1, fun);
  auto arg = fun->arg_begin();
  Assert(arg->hasStructRetAttr(), fun);
  auto alloca = builder->CreateAlloca(arg->getType(), nullptr);
  return builder->CreateCall(fun, {alloca});
}

std::string ToCoro(const std::string &name) { return name + coro_suf; }

fun_index_t CreateFunIndex(const Module &M) {
  fun_index_t index{};
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
      index.insert({annotation.str(), fun->getName()});
    }
  }
  return index;
}

// Generates
// * task_builders from generated coroutines, which has not args.
// * main where produces task builders list and call entrypoint function on it.
struct MainGenerator final {
  MainGenerator(Module &M) : M(M) {
    auto &ctx = M.getContext();
    void_t = Type::getVoidTy(ctx);
    ptr_t = PointerType::get(ctx, 0);
    i32_t = Type::getInt32Ty(ctx);

    task_t = StructType::create("Task", ptr_t);
    task_builder_list_t = ptr_t;
    task_builder_t = ptr_t;

    make_task = Function::Create(FunctionType::get(task_t, {ptr_t}, false),
                                 Function::ExternalLinkage, "make_task", M);

    new_task_builder_list =
        Function::Create(FunctionType::get(task_builder_list_t, {}, false),
                         Function::ExternalLinkage, "new_task_builder_list", M);

    destroy_task_builder_list = Function::Create(
        FunctionType::get(void_t, {task_builder_list_t}, false),
        Function::ExternalLinkage, "destroy_task_builder_list", M);

    push_task_builder_list = Function::Create(
        FunctionType::get(void_t, {task_builder_list_t, task_builder_t}, false),
        Function::ExternalLinkage, "push_task_builder_list", M);
  }

  void run(const std::string &entry_point_name,
           std::vector<Function *> &coroutines, const fun_index_t &index) {
    std::unordered_map<Type *, Function *> generators;
    for (const auto &it : index) {
      if (it.first != gen_annotation) {
        continue;
      }
      auto fun = M.getFunction(it.second);
      Assert(fun);
      if (fun->arg_size() == 0) {
        generators[fun->getReturnType()] = fun;
        continue;
      }
      errs() << fun->getName()
             << " is not valid generator: it must have 0 arguments\n";
    }

    auto &ctx = M.getContext();
    auto main = Function::Create(FunctionType::get(i32_t, {}, false),
                                 Function::ExternalLinkage, "main", M);
    auto block = BasicBlock::Create(ctx, "entry", main);
    builder_t Builder(block);

    // Create task_builder_list.
    auto task_builder_list = Builder.CreateCall(new_task_builder_list, {});

    // Push builders to list.
    for (const auto &coro : coroutines) {
      auto builder_fun =
          GenTaskBuilder(coro, coro->getName().str(), generators);
      if (builder_fun != nullptr) {
        Builder.CreateCall(push_task_builder_list,
                           {task_builder_list, builder_fun});
      }
    }

    auto entry_point = Function::Create(
        FunctionType::get(void_t, {task_builder_list_t}, false),
        Function::ExternalLinkage, entry_point_name, M);
    // Call entry point.
    Builder.CreateCall(entry_point, {task_builder_list});

    // Destroy task_builder_list.
    Builder.CreateCall(destroy_task_builder_list, {task_builder_list});
    Builder.CreateRet(ConstantInt::get(i32_t, 0));
  }

 private:
  Function *GenTaskBuilder(
      Function *F, const std::string &name,
      const std::unordered_map<Type *, Function *> &generators) {
    assert(F != nullptr && "F is nullptr");
    if (F->arg_size() != 0) {
      // Try to find generator for each argument.
      int i = 0;
      for (auto it = F->arg_begin(); it != F->arg_end(); ++it, ++i) {
        if (generators.find(it->getType()) == generators.end()) {
          errs() << "Skip TaskBuilder generation for " << F->getName()
                 << ": there is no generator for argument with index " << i
                 << "\n";
          return nullptr;
        }
      }
    }
    errs() << "Generate TaskBuilder for " << F->getName() << "\n";
    auto &ctx = M.getContext();
    auto task_builder_name = name + task_builder_suf;
    auto ftype = FunctionType::get(task_t, {}, false);

    Function *TaskBuilder = Function::Create(ftype, Function::ExternalLinkage,
                                             task_builder_name, M);

    auto block =
        BasicBlock::Create(ctx, "init", TaskBuilder, &TaskBuilder->front());
    builder_t Builder{block};
    // Generate arguments.
    std::vector<Value *> args;
    for (auto it = F->arg_begin(); it != F->arg_end(); ++it) {
      auto generator = generators.find(it->getType())->second;
      auto arg = GenerateCall(&Builder,
                              generator);  // Builder.CreateCall(generator, {});
      args.push_back(arg);
    }

    auto hdl = Builder.CreateCall(F, args);
    auto task = Builder.CreateCall(make_task, {hdl});
    Builder.CreateRet(task);

    return TaskBuilder;
  }

  Module &M;
  PointerType *ptr_t;
  Type *void_t;
  Type *i32_t;
  StructType *task_t;

  PointerType *task_builder_t;
  PointerType *task_builder_list_t;
  Function *new_task_builder_list;
  Function *destroy_task_builder_list;
  Function *push_task_builder_list;
  Function *make_task;
};

// Generates coro clones for functions in the module.
struct CoroGenerator final {
  CoroGenerator(Module &M) : M(M) {
    auto &ctx = M.getContext();
    i1_t = Type::getInt1Ty(ctx);
    i8_t = Type::getInt8Ty(ctx);
    i32_t = Type::getInt32Ty(ctx);
    ptr_t = PointerType::get(ctx, 0);
    void_t = Type::getVoidTy(ctx);
    token_t = Type::getTokenTy(ctx);
    // This signature must be as in runtime declaration.
    // TODO: validate this by some way.
    promise_t = StructType::create("CoroPromise", i32_t, i32_t, ptr_t, ptr_t);

    promise_ptr_t = PointerType::get(promise_t, 0);
    task_builder_t = ptr_t;

    // Names clashes?
    set_child_hdl = Function::Create(
        FunctionType::get(void_t, {promise_ptr_t, ptr_t}, false),
        Function::ExternalLinkage, "set_child_hdl", M);

    set_ret_val = Function::Create(
        FunctionType::get(void_t, {promise_ptr_t, i32_t}, false),
        Function::ExternalLinkage, "set_ret_val", M);

    get_ret_val =
        Function::Create(FunctionType::get(i32_t, {promise_ptr_t}, false),
                         Function::ExternalLinkage, "get_ret_val", M);
    init_promise =
        Function::Create(FunctionType::get(void_t, {ptr_t, ptr_t}, false),
                         Function::ExternalLinkage, "init_promise", M);
    get_promise =
        Function::Create(FunctionType::get(promise_ptr_t, {ptr_t}, false),
                         Function::ExternalLinkage, "get_promise", M);
  }

  std::vector<Function *> Run(const fun_index_t &index) {
    std::vector<Function *> result;
    for (auto &F : M) {
      auto name = F.getName().str();
      if (isCoroTarget(name, index)) {
        // Generate coroutine.
        auto fun = GenCoroFunc(&F, index);
        if (fun != nullptr) {
          result.push_back(fun);
        }
      }
    }
    return result;
  }

 private:
  Module &M;
  Type *void_t;
  Type *token_t;

  StructType *promise_t;

  IntegerType *i1_t;
  IntegerType *i8_t;
  IntegerType *i32_t;

  PointerType *ptr_t;
  PointerType *promise_ptr_t;
  PointerType *task_builder_t;

  Function *set_child_hdl;
  Function *set_ret_val;
  Function *get_ret_val;
  Function *get_promise;
  Function *init_promise;

  bool isCoroTarget(StringRef name, const fun_index_t &index) {
    return index.find({nonatomic_annotation, name}) != index.end();
  }

  bool needInterrupt(Instruction *insn) {
    // Suspend only after load and store.
    if (isa<LoadInst>(insn) || isa<StoreInst>(insn)) {
      return true;
    }
    return false;
  }

  Function *GenCoroFunc(Function *F, const fun_index_t &index) {
    if (F->empty()) {
      errs() << "Skip generation for " << F->getName() << ": it's empty\n";
      return nullptr;
    }
    auto coro_name = ToCoro(F->getName().str());
    SmallVector<char, 10> vec;
    if (auto func = M.getFunction(coro_name)) {
      // Was generated later.
      // TODO: what if this symbol is defined in user code?
      return func;
    }
    errs() << "Gen " << coro_name << "\n";
    auto old_ret_t = F->getReturnType();
    auto newF = utils::CloneFuncChangeRetType(F, ptr_t, coro_name);
    assert(newF != nullptr && "Generated function is nullptr");
    return rawGenCoroFunc(newF, old_ret_t, F->getName().str(), index);
  }

  // TODO: rewrite it using one pass.
  Function *rawGenCoroFunc(Function *F, Type *old_ret_t, std::string old_name,
                           const fun_index_t &index) {
    auto int_gen = utils::SeqGenerator{};
    auto &ctx = M.getContext();

    auto i32_0 = ConstantInt::get(i32_t, 0);
    auto ptr_null = ConstantPointerNull::get(ptr_t);
    auto token_none = ConstantTokenNone::get(ctx);
    auto i1_false = ConstantInt::get(i1_t, false);

    assert(ptr_t == F->getReturnType() && "F must have ptr return type");
    F->setPresplitCoroutine();

    builder_t Builder(&*F->begin());

    // * Add suspension points after suitable operations.
    for (auto b_it = (*F).begin(); b_it != (*F).end();) {
      auto cb = b_it;
      std::optional<BasicBlock *> new_entry_block;

      auto current_block = BasicBlock::Create(
          ctx, "execution." + std::to_string(int_gen.next()));
      for (auto insn_it = cb->begin(); insn_it != cb->end();) {
        if (std::next(insn_it) == cb->end()) {
          // Leave terminate instruction in this block.
          break;
        }
        auto start = insn_it;
        while (insn_it != cb->end() && !needInterrupt(&*insn_it)) {
          ++insn_it;
        }
        if (insn_it == cb->end()) {
          // This is the last segment, leave them in this block.
          break;
        }
        auto new_block = BasicBlock::Create(
            ctx, "execution." + std::to_string(int_gen.next()), F, &*b_it);
        if (!new_entry_block.has_value()) {
          new_entry_block = new_block;
        }
        b_it = cb->getIterator();
        auto iter = insn_it++;

        // Move instructions segment to the new block.
        while (true) {
          std::optional<decltype(iter)> prev;
          if (iter != start) {
            prev = std::prev(iter);
          }
          iter->removeFromParent();
          iter->insertInto(new_block, new_block->begin());
          if (!prev.has_value()) break;
          iter = prev.value();
        }
        Builder.SetInsertPoint(new_block);
        Builder.CreateIntrinsic(i8_t, Intrinsic::coro_suspend,
                                {token_none, i1_false});
      }
      b_it++;
      cb->setName("terminator." + std::to_string(int_gen.next()));
      if (new_entry_block.has_value()) {
        // Replace all uses of the terminate block with new entry block.
        cb->replaceAllUsesWith(new_entry_block.value());
        // But, phi nodes still must refer to terminate block,
        // because they are real successors.
        cb->replaceSuccessorsPhiUsesWith(new_entry_block.value(), &*cb);
      }
      // TODO: remove after DEBUG
      // int_gen.num += 100;
    }

    auto &first_real_block = F->front();
    auto suspend = BasicBlock::Create(ctx, "suspend", F, &F->front());
    auto cleanup = BasicBlock::Create(ctx, "cleanup", F, suspend);
    auto init = BasicBlock::Create(ctx, "init", F, cleanup);

    // Init:
    Builder.SetInsertPoint(init);
    auto promise = Builder.CreateAlloca(promise_t, nullptr, "promise");

    // Declare global variable that hold function name.
    auto name_const = createPrivateGlobalForString(M, old_name, false, "");
    auto name_ptr = Builder.CreateGEP(
        ArrayType::get(i8_t, old_name.length() + 1), name_const,
        {ConstantInt::get(i32_t, 0), ConstantInt::get(i32_t, 0)});

    Builder.CreateCall(init_promise, {promise, name_ptr});
    auto id = Builder.CreateIntrinsic(token_t, Intrinsic::coro_id,
                                      {i32_0, promise, ptr_null, ptr_null},
                                      nullptr, "id");
    auto size = Builder.CreateIntrinsic(i32_t, Intrinsic::coro_size, {},
                                        nullptr, "size");
    auto alloc =
        Builder.CreateCall(utils::GenMallocCallee(M), {size}, "hdl_alloc");
    auto hdl = Builder.CreateIntrinsic(ptr_t, Intrinsic::coro_begin,
                                       {id, alloc}, nullptr, "hdl");
    auto suspend_0 = Builder.CreateIntrinsic(i8_t, Intrinsic::coro_suspend,
                                             {token_none, i1_false});
    auto _switch = Builder.CreateSwitch(suspend_0, suspend, 2);
    _switch->addCase(ConstantInt::get(i8_t, 0), &first_real_block);
    _switch->addCase(ConstantInt::get(i8_t, 1), cleanup);

    // Cleanup:
    Builder.SetInsertPoint(cleanup);
    auto mem = Builder.CreateIntrinsic(ptr_t, Intrinsic::coro_free, {id, hdl});
    Builder.CreateCall(utils::GenFreeCallee(M), {mem});
    Builder.CreateBr(suspend);

    // Suspend:
    Builder.SetInsertPoint(suspend);
    auto unused = Builder.CreateIntrinsic(i1_t, Intrinsic::coro_end,
                                          {hdl, i1_false, token_none});
    Builder.CreateRet(hdl);

    // * Replace ret instructions with result memorization.
    // We memorize return value in promise.
    // * Create terminate instructions in execution blocks.
    // * Replace function calls with coroutine clone calls.
    for (auto &b : *F) {
      auto b_name = b.getName();

      if (!b_name.starts_with("execution") && !b_name.starts_with("init") &&
          !b_name.starts_with("cleanup") && !b_name.starts_with("suspend")) {
        auto instr = &*b.rbegin();
        // Check if terminate instruction is `ret`.
        if (auto ret = dyn_cast<ReturnInst>(instr)) {
          Builder.SetInsertPoint(ret);
          if (old_ret_t == i32_t) {
            Builder.CreateCall(set_ret_val, {promise, ret->getOperand(0)});
          } else {
            // If function returns not int we set i32 as ret value.
            // TODO: Take hash if we can.
            Builder.CreateCall(set_ret_val, {promise, i32_0});
          }
          // We need to suspend after `ret` because
          // return value must be accessed before coroutine destruction.
          auto suspend_res = Builder.CreateIntrinsic(
              i8_t, Intrinsic::coro_suspend, {token_none, i1_false});
          auto _switch = Builder.CreateSwitch(suspend_res, suspend, 2);
          _switch->addCase(ConstantInt::get(i8_t, 0), cleanup);
          _switch->addCase(ConstantInt::get(i8_t, 1), cleanup);
          instr->eraseFromParent();
        }
      }

      if (!b_name.starts_with("execution")) {
        continue;
      }

      auto suspend_res = &*b.rbegin();
      Builder.SetInsertPoint(&b);
      auto _switch = Builder.CreateSwitch(suspend_res, suspend, 2);

      auto next_block = &*std::next(b.getIterator());
      _switch->addCase(ConstantInt::get(i8_t, 0), next_block);
      _switch->addCase(ConstantInt::get(i8_t, 1), cleanup);

      auto insn = &*std::prev(suspend_res->getIterator());
      if (auto call = dyn_cast<CallInst>(insn)) {
        // Replace calls with coroutines clone calls.
        // Maybe need to generate coro clones in process.
        auto f = call->getCalledFunction();
        if (!f || !f->hasName()) {
          continue;
        }
        auto f_name = f->getName().str();
        if (isCoroTarget(f_name, index)) {
          // TODO: remove after debug.
          errs() << "Replace " << f_name << " call\n";
          auto coro_f = GenCoroFunc(f, index);
          if (coro_f == nullptr) {
            // We can't generate this call because
            // can't generate coro clone for callee.
            continue;
          }

          Builder.SetInsertPoint(call);
          std::vector<Value *> args(call->arg_size());
          for (int i = 0; i < call->arg_size(); ++i) {
            args[i] = call->getArgOperand(i);
          }
          auto hdl = Builder.CreateCall(coro_f, args);

          Builder.SetInsertPoint(call);
          Builder.CreateCall(set_child_hdl, {promise, hdl});
          // %hdl = ptr call @call_coro()
          // %suspend = @llvm.coro.suspend()
          // switch
          //
          // In the next block:
          // %as_promise = call Promise* @get_promise(hdl)
          // %ret_val    = call i32 @get_ret(as_promise)
          // operate with %ret_val

          Builder.SetInsertPoint(&*next_block->begin());
          auto as_promise = Builder.CreateCall(get_promise, {hdl});
          auto ret_val = Builder.CreateCall(get_ret_val, {as_promise});
          for (auto &use : call->uses()) {
            User *user = use.getUser();
            user->setOperand(use.getOperandNo(), ret_val);
          }
          call->eraseFromParent();
        }
      }
    }

    return F;
  }
};

namespace {

struct CoroGenPass : public PassInfoMixin<CoroGenPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    auto fun_index = CreateFunIndex(M);

    CoroGenerator gen{M};
    auto coroutines = gen.Run(fun_index);

    MainGenerator main_gen{M};
    main_gen.run("run", coroutines, fun_index);

    return PreservedAnalyses::none();
  };
};

}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {.APIVersion = LLVM_PLUGIN_API_VERSION,
          .PluginName = "coro_gen",
          .PluginVersion = "v0.1",
          .RegisterPassBuilderCallbacks = [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "coro_gen") {
                    MPM.addPass(CoroGenPass{});
                    return true;
                  }
                  return false;
                });
          }};
}
