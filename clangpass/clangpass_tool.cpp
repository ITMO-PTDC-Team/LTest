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
#include <clang/Basic/Diagnostic.h>
#include <clang/Tooling/Refactoring/Rename/RenamingAction.h>
#include <clang/Tooling/Refactoring/Rename/USRFindingAction.h>
#include <iostream>
#include "clangpass.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;

//===----------------------------------------------------------------------===//
// Command line options
//===----------------------------------------------------------------------===//
static cl::OptionCategory CodeRefactorCategory("atomics-replacer options");

// TODO: make the prefix (e.g. `__tmp_`source.cpp) a external parameter as well
static cl::list<std::string> ClassNameToReplaceOpt{
  "replace-names",
  cl::desc("Names of the classes/structs which usages should be renamed"),
  cl::OneOrMore, cl::cat(CodeRefactorCategory)};
static cl::list<std::string> ClassNameToInsertOpt{
  "insert-names", cl::desc("Names of the classes/structs which should be used instead"),
  cl::OneOrMore, cl::cat(CodeRefactorCategory)};

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
      CI.getASTContext(), RewriterForCodeRefactor, 
      std::vector<std::string>(ClassNameToReplaceOpt.begin(), ClassNameToReplaceOpt.end()), ClassNameToInsertOpt);
  }

private:
  Rewriter RewriterForCodeRefactor;
};


class MyDiagnosticConsumer : public DiagnosticConsumer {
  public:
      void HandleDiagnostic(DiagnosticsEngine::Level Level,
                           const Diagnostic &Info) override {
          // Custom handling of diagnostics
          llvm::SmallString<100> OutStr;
          Info.FormatDiagnostic(OutStr);
          llvm::errs() << "MyConsumer: " << OutStr << "\n";
      }
      
      // Optional: Override to be informed when a source file is processed
      void clear() override { /* ... */ }
      
      // Optional: Override to control diagnostic output
      bool IncludeInDiagnosticCounts() const override { return true; }
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

  auto Files = eOptParser->getSourcePathList();
  clang::tooling::RefactoringTool Tool(
    eOptParser->getCompilations(),
    eOptParser->getSourcePathList()
  );
  tooling::USRFindingAction FindingAction({}, ClassNameToReplaceOpt, false);
  Tool.run(tooling::newFrontendActionFactory(&FindingAction).get());
  const std::vector<std::vector<std::string>> &USRList =
      FindingAction.getUSRList();
  const std::vector<std::string> &PrevNames = FindingAction.getUSRSpellings();
  if (FindingAction.errorOccurred()) {
    // Diagnostics are already issued at this point.
    return 1;
  }

  for (auto i = 0; i < Files.size(); ++i) {
    std::cerr << "FILE: " << Files[i] << "\n";
  }

  for (auto i = 0; i < USRList.size(); ++i) {
    for (auto j : USRList[i]) {
      std::cerr << j << "\n";
    }
  }

  // Perform the renaming.
  tooling::RenamingAction RenameAction(ClassNameToInsertOpt, PrevNames, USRList,
                                       Tool.getReplacements(), true);
  std::unique_ptr<tooling::FrontendActionFactory> Factory =
      tooling::newFrontendActionFactory(&RenameAction);
  int ExitCode = Tool.run(Factory.get());

  // Write every file to stdout. Right now we just barf the files without any
    // indication of which files start where, other than that we print the files
    // in the same order we see them.
    LangOptions DefaultLangOptions;
    IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
    IgnoringDiagConsumer DiagnosticConsumer;
    DiagnosticsEngine Diagnostics(
        IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()), &*DiagOpts,
        &DiagnosticConsumer, false);
    auto &FileMgr = Tool.getFiles();
    SourceManager Sources(Diagnostics, FileMgr);
    Rewriter Rewrite(Sources, DefaultLangOptions);

    Tool.applyAllReplacements(Rewrite);
    for (const auto &File : Files) {
      auto EntryRef = FileMgr.getFileRef(File);
      const auto ID = Sources.getOrCreateFileID(*EntryRef, SrcMgr::C_User);
      // Rewrite.getEditBuffer(ID).write(outs());
      const RewriteBuffer& Buffer = Rewrite.getEditBuffer(ID);

      // Output to stdout
      llvm::outs() << "Transformed code:\n";
      Buffer.write(llvm::outs());
      llvm::outs() << "\n";

      // Output to file
      const FileEntry *Entry = Sources.getFileEntryForID(ID);
      StringRef OriginalFilename = Entry->tryGetRealPathName();

      size_t slashIndex = OriginalFilename.rfind("/");
      // the path should be absolute, so in the worst case we will get '/' as index 0
      assert(slashIndex != std::string::npos);
      slashIndex += 1; // include the '/' itself

      std::string Path = std::string(OriginalFilename.begin(), OriginalFilename.begin() + slashIndex);
      std::string SourceFilename = std::string(OriginalFilename.begin() + slashIndex, OriginalFilename.end());

      llvm::outs() << "Original filename: " << OriginalFilename << "\n";
      std::string OutputFilename = Path + "__tmp_" + SourceFilename;

      std::error_code EC;
      llvm::raw_fd_ostream OS(OutputFilename, EC, llvm::sys::fs::OF_None);
      
      if (EC) {
        llvm::errs() << "Error: Could not open output file: " << EC.message() << "\n";
        return EC.value();
      }

      llvm::outs() << "Writing to file: " << OutputFilename << "\n";
      //OS << std::string(Buffer->begin(), Buffer->end());
      Buffer.write(OS);
      OS.close();
    }

  return ExitCode;


  // return Tool.run(clang::tooling::newFrontendActionFactory<CodeRefactorPluginAction>().get());
}
