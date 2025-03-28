//==============================================================================
// FILE:
//    clangpass_tool.cpp
//
// DESCRIPTION:
//    Replaces all ::std:: names usages to custom runtime friendly implementations  
//
// USAGE:
//    * ./build/bin/clangpass_tool --replace-names=::std::atomic,::std::mutex --insert-names=LTestAtomic,ltest::mutex ./verifying/targets/nonlinear_queue.cpp
//
// License: The Unlicense
//==============================================================================
#include <clang/Basic/Diagnostic.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/Refactoring/Rename/RenamingAction.h>
#include <clang/Tooling/Refactoring/Rename/USRFindingAction.h>
#include "clangpass.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;
using namespace replace_pass;

//===----------------------------------------------------------------------===//
// Command line options
//===----------------------------------------------------------------------===//
static cl::OptionCategory CodeRefactorCategory("atomics-replacer options");

static cl::opt<std::string> TemporaryPrefix{
  "temp-prefix",
  cl::desc("Prefix for temporary files"),
  cl::init("__tmp_"), 
  cl::cat(CodeRefactorCategory)};
static cl::list<std::string> ClassNamesToReplace{
  "replace-names",
  cl::desc("Names of the classes/structs which usages should be renamed"),
  cl::OneOrMore,
  cl::CommaSeparated,
  cl::cat(CodeRefactorCategory)};
static cl::list<std::string> ClassNamesToInsert{
  "insert-names",
  cl::desc("Names of the classes/structs which should be used instead"),
  cl::OneOrMore, 
  cl::CommaSeparated,
  cl::cat(CodeRefactorCategory)};

//===----------------------------------------------------------------------===//
// PluginASTAction
//===----------------------------------------------------------------------===//
class CodeRefactorPluginAction : public PluginASTAction {
public:
  explicit CodeRefactorPluginAction() {}
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
    RewriterForCodeRefactor.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());

    std::vector<ReplacePair> pairs;
    pairs.reserve(ClassNamesToReplace.size());
    for (int i = 0; i < ClassNamesToReplace.size(); ++i) {
      pairs.emplace_back(ClassNamesToReplace[i], ClassNamesToInsert[i]);
    }

    return std::make_unique<CodeRefactorASTConsumer>(
      CI.getASTContext(), RewriterForCodeRefactor, pairs);
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

  // auto files = eOptParser->getSourcePathList();
  // std::vector<const char*> Args = {"clang++", "-E" };
  // Args.insert(Args.end(), files.begin(), files.end());
    
  // auto OptionsParser = clang::tooling::CommonOptionsParser::create(
  //     Args.size(), Args.data(), 
  //   );
  
  // clang::tooling::ClangTool Tool(OptionsParser->getCompilations(),
  //                              OptionsParser->getSourcePathList());

  clang::tooling::RefactoringTool Tool(
    eOptParser->getCompilations(),
    eOptParser->getSourcePathList()
  );

  return Tool.run(clang::tooling::newFrontendActionFactory<CodeRefactorPluginAction>()
  .get());

}
