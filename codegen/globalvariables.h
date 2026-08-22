#pragma once

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/Demangle/Demangle.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include <string>

namespace ltest {

inline bool IsLtestGlobal(const llvm::GlobalValue& value) {
  if (value.getName().starts_with("llvm.") ||
      value.getSection() == "ltest_registration_globals") {
    return true;
  }
  auto demangled = llvm::demangle(value.getName().str());
  return demangled.rfind("ltest::", 0) == 0 ||
         demangled.find(" ltest::") != std::string::npos ||
         demangled == "std::__ioinit";
}

inline llvm::GlobalVariable* GetUnderlyingGlobal(llvm::Value* value) {
  if (value == nullptr || !value->getType()->isPointerTy()) {
    return nullptr;
  }
  llvm::Value* underlying = llvm::getUnderlyingObject(value);
  if (auto* alias = llvm::dyn_cast<llvm::GlobalAlias>(underlying)) {
    underlying = alias->getAliaseeObject();
  }
  return llvm::dyn_cast_or_null<llvm::GlobalVariable>(underlying);
}

struct GlobalReplacer {
  explicit GlobalReplacer(llvm::Module& module) : module(module) {}

  void Run() {
    auto* pointer = llvm::PointerType::get(module.getContext(), 0);
    llvm::SmallVector<llvm::Constant*> reset_entries;
    llvm::SmallVector<llvm::Constant*> init_entries;

    FindProcessLifetimeGlobals();
    CreateGlobalResetters(pointer, reset_entries);
    ExtractGlobalConstructors(init_entries);
    RemoveGlobalDestructors();
    CreateFunctionArrays(pointer, reset_entries, "ltest_reset_array",
                         "ltest_reset");
    CreateFunctionArrays(pointer, init_entries, "ltest_init_array",
                         "ltest_init");
  }

 private:
  void CreateGlobalResetters(llvm::PointerType* pointer,
                             llvm::SmallVectorImpl<llvm::Constant*>& entries) {
    // Restore constant initial values before replaying dynamic constructors.
    for (auto& global : module.globals()) {
      if (global.isExternallyInitialized() || global.isConstant() ||
          global.hasAppendingLinkage() || !global.hasInitializer() ||
          IsLtestGlobal(global) ||
          process_lifetime_globals.contains(&global)) {
        continue;
      }

      auto* resetter = llvm::Function::Create(
          llvm::FunctionType::get(llvm::Type::getVoidTy(module.getContext()),
                                  {}, false),
          llvm::GlobalValue::InternalLinkage, "", module);
      resetter->addFnAttr(llvm::Attribute::NoInline);
      if (global.hasComdat()) {
        resetter->setComdat(global.getComdat());
      }
      auto* block = llvm::BasicBlock::Create(module.getContext(), "", resetter);
      llvm::IRBuilder<> builder(block);
      builder.CreateStore(global.getInitializer(), &global);
      builder.CreateRetVoid();

      entries.push_back(llvm::ConstantExpr::getPointerCast(resetter, pointer));
      resettable_globals.insert(&global);
    }
  }

  bool ReferencesResettableGlobal(const llvm::Function& function) const {
    for (const auto& block : function) {
      for (const auto& instruction : block) {
        for (const llvm::Use& operand : instruction.operands()) {
          auto* global = GetUnderlyingGlobal(operand.get());
          if (global != nullptr && resettable_globals.contains(global)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  template <class Callback>
  static void ForEachDirectCallee(const llvm::Function& function,
                                  Callback callback) {
    for (const auto& block : function) {
      for (const auto& instruction : block) {
        auto* call = llvm::dyn_cast<llvm::CallBase>(&instruction);
        if (call == nullptr) {
          continue;
        }
        auto* callee = llvm::dyn_cast<llvm::Function>(
            call->getCalledOperand()->stripPointerCasts());
        if (callee != nullptr) {
          callback(*callee);
        }
      }
    }
  }

  static bool RegistersGflag(const llvm::Function& function) {
    bool found = false;
    ForEachDirectCallee(function, [&](const llvm::Function& callee) {
      const auto name = llvm::demangle(callee.getName().str());
      found |= name.rfind("google::FlagRegisterer::FlagRegisterer", 0) == 0;
    });
    return found;
  }

  bool AddProcessLifetimeGlobals(const llvm::Function& initializer) {
    if (!RegistersGflag(initializer)) {
      return false;
    }
    for (const auto& block : initializer) {
      for (const auto& instruction : block) {
        for (const llvm::Use& operand : instruction.operands()) {
          if (auto* global = GetUnderlyingGlobal(operand.get())) {
            process_lifetime_globals.insert(global);
          }
        }
      }
    }
    return true;
  }

  void FindProcessLifetimeGlobals() {
    auto* constructors = module.getNamedGlobal("llvm.global_ctors");
    if (constructors == nullptr || !constructors->hasInitializer()) {
      return;
    }
    auto* array = llvm::dyn_cast<llvm::ConstantArray>(
        constructors->getInitializer());
    if (array == nullptr) {
      return;
    }

    for (llvm::Value* operand : array->operands()) {
      auto* entry = llvm::cast<llvm::Constant>(operand);
      auto* function = GetEntryFunction(entry);
      if (function == nullptr) {
        continue;
      }

      auto* object = GetEntryObject(entry);
      if (object != nullptr || function->getSection() != ".text.startup") {
        if (AddProcessLifetimeGlobals(*function) && object != nullptr) {
          process_lifetime_globals.insert(object);
        }
        continue;
      }

      ForEachDirectCallee(*function, [&](const llvm::Function& initializer) {
        AddProcessLifetimeGlobals(initializer);
      });
    }
  }

  static llvm::Function* GetEntryFunction(llvm::Constant* entry) {
    auto* fields = llvm::dyn_cast<llvm::ConstantStruct>(entry);
    if (fields == nullptr || fields->getNumOperands() < 2) {
      return nullptr;
    }
    return llvm::dyn_cast<llvm::Function>(
        fields->getOperand(1)->stripPointerCasts());
  }

  static llvm::GlobalVariable* GetEntryObject(llvm::Constant* entry) {
    auto* fields = llvm::dyn_cast<llvm::ConstantStruct>(entry);
    if (fields == nullptr || fields->getNumOperands() < 3 ||
        fields->getOperand(2)->isNullValue()) {
      return nullptr;
    }
    return GetUnderlyingGlobal(fields->getOperand(2));
  }

  template <class Callback>
  void ExtractResettableCalls(llvm::Function& wrapper, Callback callback) {
    llvm::SmallVector<llvm::CallInst*> extracted_calls;
    for (auto& block : wrapper) {
      for (auto& instruction : block) {
        auto* call = llvm::dyn_cast<llvm::CallInst>(&instruction);
        if (call == nullptr) {
          continue;
        }
        auto* callee = llvm::dyn_cast<llvm::Function>(
            call->getCalledOperand()->stripPointerCasts());
        if (callee == nullptr || callee == &wrapper ||
            !ReferencesResettableGlobal(*callee)) {
          continue;
        }
        callback(*callee);
        extracted_calls.push_back(call);
      }
    }
    for (auto* call : extracted_calls) {
      call->eraseFromParent();
    }
  }

  void ExtractGlobalConstructors(
      llvm::SmallVectorImpl<llvm::Constant*>& init_entries) {
    auto* constructors = module.getNamedGlobal("llvm.global_ctors");
    if (constructors == nullptr || !constructors->hasInitializer()) {
      return;
    }
    auto* array =
        llvm::dyn_cast<llvm::ConstantArray>(constructors->getInitializer());
    if (array == nullptr) {
      return;
    }

    llvm::SmallVector<llvm::Constant*> kept;
    llvm::SmallPtrSet<llvm::Function*, 16> added;
    auto add_constructor = [&](llvm::Function& function) {
      if (added.insert(&function).second) {
        init_entries.push_back(llvm::ConstantExpr::getPointerCast(
            &function, llvm::PointerType::get(module.getContext(), 0)));
      }
    };

    for (llvm::Value* operand : array->operands()) {
      auto* entry = llvm::cast<llvm::Constant>(operand);
      auto* function = GetEntryFunction(entry);
      auto* object = GetEntryObject(entry);
      const bool associated_with_resettable =
          object != nullptr && resettable_globals.contains(object);

      if (function == nullptr) {
        kept.push_back(entry);
      } else if (associated_with_resettable) {
        add_constructor(*function);
      } else if (object == nullptr &&
                 function->getSection() == ".text.startup") {
        ExtractResettableCalls(*function, add_constructor);
        kept.push_back(entry);
      } else if (ReferencesResettableGlobal(*function)) {
        add_constructor(*function);
      } else {
        kept.push_back(entry);
      }
    }
    RebuildGlobalList(*constructors, kept);
  }

  void RemoveGlobalDestructors() {
    auto* destructors = module.getNamedGlobal("llvm.global_dtors");
    if (destructors == nullptr || !destructors->hasInitializer()) {
      return;
    }
    // LTest abandons all process objects; preload applies the same policy to
    // __cxa_atexit registrations.
    RebuildGlobalList(*destructors, {});
  }

  void RebuildGlobalList(llvm::GlobalVariable& list,
                         llvm::ArrayRef<llvm::Constant*> entries) {
    auto* old_array = llvm::cast<llvm::ArrayType>(list.getValueType());
    if (entries.size() == old_array->getNumElements()) {
      return;
    }

    std::string name = list.getName().str();
    list.setName(name + ".old");
    auto* type =
        llvm::ArrayType::get(old_array->getElementType(), entries.size());
    auto* replacement = new llvm::GlobalVariable(
        module, type, list.isConstant(), list.getLinkage(),
        llvm::ConstantArray::get(type, entries), name);
    replacement->copyAttributesFrom(&list);
    list.replaceAllUsesWith(replacement);
    list.eraseFromParent();
  }

  void CreateFunctionArrays(
      llvm::PointerType* pointer,
      const llvm::SmallVectorImpl<llvm::Constant*>& entries,
      llvm::StringRef array_name, llvm::StringRef section_name) {
    llvm::SmallVector<llvm::Constant*> ungrouped;
    llvm::DenseMap<llvm::Comdat*, llvm::SmallVector<llvm::Constant*>> grouped;
    for (auto* entry : entries) {
      auto* object =
          llvm::dyn_cast<llvm::GlobalObject>(entry->stripPointerCasts());
      if (object != nullptr && object->hasComdat()) {
        grouped[object->getComdat()].push_back(entry);
      } else {
        ungrouped.push_back(entry);
      }
    }

    CreateFunctionArray(pointer, ungrouped, nullptr, array_name, section_name);
    for (auto& [comdat, group_entries] : grouped) {
      CreateFunctionArray(pointer, group_entries, comdat, array_name,
                          section_name);
    }
  }

  void CreateFunctionArray(llvm::PointerType* pointer,
                           llvm::ArrayRef<llvm::Constant*> entries,
                           llvm::Comdat* comdat, llvm::StringRef array_name,
                           llvm::StringRef section_name) {
    auto* type = llvm::ArrayType::get(pointer, entries.size());
    auto* array = new llvm::GlobalVariable(
        module, type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(type, entries), array_name);
    array->setSection(section_name);
    if (comdat != nullptr) {
      array->setComdat(comdat);
    }
    llvm::appendToCompilerUsed(module, {array});
  }

  llvm::Module& module;
  llvm::SmallPtrSet<llvm::GlobalVariable*, 32> resettable_globals;
  llvm::SmallPtrSet<const llvm::GlobalVariable*, 16>
      process_lifetime_globals;
};

struct GlobalVarsPass final : public llvm::PassInfoMixin<GlobalVarsPass> {
  llvm::PreservedAnalyses run(llvm::Module& module,
                              llvm::ModuleAnalysisManager&) {
    GlobalReplacer replacer(module);
    replacer.Run();
    if (verifyModule(module, &llvm::errs())) {
      llvm::report_fatal_error("module verification failed", false);
    }
    return llvm::PreservedAnalyses::none();
  }
};

}  // namespace ltest
