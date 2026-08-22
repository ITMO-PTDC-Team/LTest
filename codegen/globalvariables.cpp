#include "globalvariables.h"

#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {
      .APIVersion = LLVM_PLUGIN_API_VERSION,
      .PluginName = "global_variables",
      .PluginVersion = "v0.1",
      .RegisterPassBuilderCallbacks = [](llvm::PassBuilder& builder) {
        builder.registerPipelineStartEPCallback(
            [](llvm::ModulePassManager& manager,
               llvm::OptimizationLevel) {
              manager.addPass(ltest::GlobalVarsPass());
            });
      }};
}
