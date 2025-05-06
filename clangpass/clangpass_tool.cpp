//==============================================================================
// FILE:
//    clangpass_tool.cpp
//
// DESCRIPTION:
//    Replaces all ::std:: names usages to custom runtime friendly
//    implementations
//
// USAGE:
//    * ./build/bin/clangpass_tool --replace-names=::std::atomic,::std::mutex
//    --insert-names=LTestAtomic,ltest::mutex
//    ./verifying/targets/nonlinear_queue.cpp
//
// License: The Unlicense
//==============================================================================
#include <clang/Basic/Diagnostic.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/Refactoring/Rename/RenamingAction.h>
#include <clang/Tooling/Refactoring/Rename/USRFindingAction.h>

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "include/clangpass.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;
using namespace replace_pass;

//===----------------------------------------------------------------------===//
// Command line options
//===----------------------------------------------------------------------===//
static cl::OptionCategory CodeRefactorCategory("atomics-replacer options");

static cl::opt<std::string> TemporaryPrefix{
    "temp-prefix", cl::desc("Prefix for temporary files"), cl::init("__tmp_"),
    cl::cat(CodeRefactorCategory)};
static cl::list<std::string> ClassNamesToReplace{
    "replace-names",
    cl::desc("Names of the classes/structs which usages should be renamed"),
    cl::OneOrMore, cl::CommaSeparated, cl::cat(CodeRefactorCategory)};
static cl::list<std::string> ClassNamesToInsert{
    "insert-names",
    cl::desc("Names of the classes/structs which should be used instead"),
    cl::OneOrMore, cl::CommaSeparated, cl::cat(CodeRefactorCategory)};

//===----------------------------------------------------------------------===//
// PluginASTAction
//===----------------------------------------------------------------------===//
class CodeRefactorPluginAction : public PluginASTAction {
 public:
  explicit CodeRefactorPluginAction() {}
  // Not used
  bool ParseArgs(const CompilerInstance &compiler,
                 const std::vector<std::string> &args) override {
    return true;
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &compiler,
                                                 StringRef file) override {
    rewriter.setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());

    std::vector<ReplacePair> pairs;
    pairs.reserve(ClassNamesToReplace.size());
    for (int i = 0; i < ClassNamesToReplace.size(); ++i) {
      pairs.emplace_back(ClassNamesToReplace[i], ClassNamesToInsert[i]);
    }

    return std::make_unique<CodeRefactorASTConsumer>(
        compiler.getASTContext(), rewriter, pairs, TemporaryPrefix);
  }

 private:
  Rewriter rewriter;
};

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
int main(int argc, const char **argv) {
  Expected<tooling::CommonOptionsParser> options =
      clang::tooling::CommonOptionsParser::create(argc, argv,
                                                  CodeRefactorCategory);
  if (auto E = options.takeError()) {
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

  clang::tooling::RefactoringTool tool(options->getCompilations(),
                                       options->getSourcePathList());

  return tool.run(
      clang::tooling::newFrontendActionFactory<CodeRefactorPluginAction>()
          .get());
}
