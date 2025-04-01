//==============================================================================
// FILE:
//    clangpass_tool.cpp
//
// DESCRIPTION:
//    A standalone tool that runs the clangpass plugin. See
//    clangpass.cpp for a complete description.
//
// USAGE:
//    * ./build/bin/clangpass_tool --replace-name=::std::atomic --insert-name=LTestAtomic ./AtomicsReplacer/test-project/main.cpp
//
//
// License: The Unlicense
//==============================================================================
#include "clangpass.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace clang;

//===----------------------------------------------------------------------===//
// Command line options
//===----------------------------------------------------------------------===//
static cl::OptionCategory CodeRefactorCategory("atomics-replacer options");

// TODO: make the prefix (e.g. `__tmp_`source.cpp) a external parameter as well
static cl::opt<std::string> ClassNameToReplaceOpt{
  "replace-name",
  cl::desc("The name of the class/struct which usages should be renamed"),
  cl::Required, cl::init(""), cl::cat(CodeRefactorCategory)};
static cl::opt<std::string> ClassNameToInsertOpt{
  "insert-name", cl::desc("The name of the class/struct which should be used instead"),
  cl::Required, cl::init(""), cl::cat(CodeRefactorCategory)};

//===----------------------------------------------------------------------===//
// PluginASTAction
//===----------------------------------------------------------------------===//
class CodeRefactorPluginAction : public PluginASTAction {
public:
  explicit CodeRefactorPluginAction() {};
  // Not used
  bool ParseArgs(
    const CompilerInstance &CI,
    const std::vector<std::string> &args
  ) override {
    return true;
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(
    CompilerInstance &CI,
    StringRef file
  ) override {
    RewriterForCodeRefactor.setSourceMgr(
      CI.getSourceManager(),
      CI.getLangOpts()
    );
    return std::make_unique<CodeRefactorASTConsumer>(
      CI.getASTContext(), RewriterForCodeRefactor, ClassNameToReplaceOpt, ClassNameToInsertOpt);
  }

private:
  Rewriter RewriterForCodeRefactor;
};

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
int main(int Argc, const char **Argv) {
  Expected<tooling::CommonOptionsParser> eOptParser =
    clang::tooling::CommonOptionsParser::create(
      Argc,
      Argv,
      CodeRefactorCategory
    );
  if (auto E = eOptParser.takeError()) {
    errs() << "Problem constructing CommonOptionsParser "
           << toString(std::move(E)) << '\n';
    return EXIT_FAILURE;
  }
  clang::tooling::RefactoringTool Tool(
    eOptParser->getCompilations(),
    eOptParser->getSourcePathList()
  );

  return Tool.run(clang::tooling::newFrontendActionFactory<CodeRefactorPluginAction>().get());
}
