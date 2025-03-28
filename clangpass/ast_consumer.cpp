
#include "include/clangpass.h"

replace_pass::CodeRefactorASTConsumer::CodeRefactorASTConsumer(
    ASTContext& Context,
    clang::Rewriter &R,
    std::vector<ReplacePair> Names
  ): CodeRefactorHandler(Context, R, Names),
      Names(Names),
      Rewriter(R) {
  
    auto factory = NameMatcherFactory();
    
    for (auto name: Names) {
      const auto MatcherForFQTemplateTypes = factory.CreateMatcherFor(name.old_name);
      // Uses previous matcher inside, but returns a wrapping QualifiedTypeLoc node
      // which is used in the function parameters
      const auto MatcherForFQTemplateParams = qualifiedTypeLoc(
        hasUnqualifiedLoc(
          MatcherForFQTemplateTypes
        )
      );
      Finder.addMatcher(MatcherForFQTemplateTypes.bind("ElaboratedTypeLoc" + name.old_name), &CodeRefactorHandler);
      Finder.addMatcher(MatcherForFQTemplateParams.bind("QualifiedTypeLoc" + name.old_name), &CodeRefactorHandler);
    }
  }
  
void replace_pass::CodeRefactorASTConsumer::HandleTranslationUnit(clang::ASTContext &Ctx) {
  Finder.matchAST(Ctx);
  const SourceManager& SM = Rewriter.getSourceMgr();    
  auto ID = SM.getMainFileID();
  // Rewrite.getEditBuffer(ID).write(outs());
  const RewriteBuffer& Buffer = Rewriter.getEditBuffer(ID);

  // Output to stdout
  llvm::outs() << "Transformed code:\n";
  Buffer.write(llvm::outs());
  llvm::outs() << "\n";

  // Output to file
  const FileEntry *Entry = SM.getFileEntryForID(ID);
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
      return;
  }

  llvm::outs() << "Writing to file: " << OutputFilename << "\n";
  //OS << std::string(Buffer->begin(), Buffer->end());
  Buffer.write(OS);
  OS.close();
}
