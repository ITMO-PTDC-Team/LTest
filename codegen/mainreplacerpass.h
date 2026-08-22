#pragma once
#include <llvm/Demangle/Demangle.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
/***
 * This pass changes name of main to test_main an normalizes it signature to
 *(argc, argv) mode and inserts new main which calls ltest_main
 ***/

static constexpr std::string_view main_name = "main";
static constexpr std::string_view test_name = "test_main";
static constexpr std::string_view ltest_main_name = "ltest_main";

class MainReplacer {
  llvm::Module& module;
  llvm::FunctionType* main_type;
  void CreateNewMain() const {
    auto ltest_main = llvm::Function::Create(
        main_type, llvm::GlobalValue::ExternalWeakLinkage, ltest_main_name, module);
    llvm::IRBuilder builder(module.getContext());
    auto real_main = llvm::Function::Create(
        main_type, llvm::GlobalValue::ExternalLinkage, main_name, module);
    real_main->setDSOLocal(true);
    AddAttrs(real_main);

    auto bb = llvm::BasicBlock::Create(module.getContext(), "", real_main);
    builder.SetInsertPoint(bb);
    auto ret = builder.CreateCall(ltest_main,
                                  {real_main->getArg(0), real_main->getArg(1)});
    builder.CreateRet(ret);
  }
  // Convert(1) to (2) https://en.cppreference.com/cpp/language/main_function
  void NormalizeMain(llvm::Function* f) const {
    auto& context = module.getContext();
    auto new_main = llvm::Function::Create(
        main_type, llvm::GlobalValue::ExternalLinkage, test_name, module);
    llvm::ValueToValueMapTy value_map;
    llvm::SmallVector<llvm::ReturnInst*, 1> Returns;

    llvm::CloneFunctionInto(new_main, f, value_map,
                            llvm::CloneFunctionChangeType::LocalChangesOnly,
                            Returns);
    AddAttrs(new_main);
    f->eraseFromParent();
  }
  static void AddAttrs(llvm::Function* f) {
    f->addParamAttr(0, llvm::Attribute::NoUndef);
    f->addParamAttr(1, llvm::Attribute::NoUndef);
    f->addRetAttr(llvm::Attribute::NoUndef);
  }

 public:
  explicit MainReplacer(llvm::Module& module) : module(module) {
    auto& context = module.getContext();
    llvm::IntegerType* i32 = llvm::IntegerType::getInt32Ty(context);
    main_type = llvm::FunctionType::get(
        i32, {i32, llvm::PointerType::get(context, 0)}, false);
  }
  void Run() const {
    for (auto& f : module) {
      if (f.isDeclaration()) {
        continue;
      }
      auto name = llvm::demangle(f.getName());
      if (name == main_name) {
        if (f.arg_size() == 2) {
          f.setName(test_name);
          f.setLinkage(llvm::GlobalValue::ExternalLinkage);
        } else {
          NormalizeMain(&f);
        }
        CreateNewMain();
        break;
      }
    }
  }
};

struct MainReplacerPass final : llvm::PassInfoMixin<MainReplacerPass> {
  llvm::PreservedAnalyses run(llvm::Module& M,
                              llvm::ModuleAnalysisManager& AM) {
    MainReplacer replacer(M);
    replacer.Run();
    if (verifyModule(M, &llvm::errs())) {
      llvm::report_fatal_error("module verification failed", false);
    }
    return llvm::PreservedAnalyses::none();
  };
};