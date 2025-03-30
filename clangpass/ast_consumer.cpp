
#include "include/clangpass.h"

replace_pass::CodeRefactorASTConsumer::CodeRefactorASTConsumer(
    ASTContext& context, clang::Rewriter& rewriter,
    std::vector<ReplacePair> names, std::string temp_prefix)
    : refactor_handler(context, rewriter, names),
      names(names),
      rewriter(rewriter),
      temp_prefix(std::move(temp_prefix)) {
  auto factory = NameMatcherFactory();

  for (auto name : names) {
    const auto elaborated_matcher = factory.CreateMatcherFor(name.old_name);
    // Uses previous matcher inside, but returns a wrapping QualifiedTypeLoc
    // node which is used in the function parameters
    const auto qualified_matcher =
        qualifiedTypeLoc(hasUnqualifiedLoc(elaborated_matcher));
    match_finder.addMatcher(
        elaborated_matcher.bind("ElaboratedTypeLoc" + name.old_name),
        &refactor_handler);
    match_finder.addMatcher(
        qualified_matcher.bind("QualifiedTypeLoc" + name.old_name),
        &refactor_handler);
  }
}

std::string replace_pass::CodeRefactorASTConsumer::RefactoredFileName(
    StringRef original_filename) const {
  size_t slash_index = original_filename.rfind("/");
  // the path should be absolute, so in the worst case we will get '/' as index
  // 0
  assert(slash_index != std::string::npos);
  slash_index += 1;  // include the '/' itself

  std::string path = std::string(original_filename.begin(),
                                 original_filename.begin() + slash_index);
  std::string filename = std::string(original_filename.begin() + slash_index,
                                     original_filename.end());

  return path + temp_prefix + filename;
}

void replace_pass::CodeRefactorASTConsumer::HandleTranslationUnit(
    clang::ASTContext& ctx) {
  match_finder.matchAST(ctx);
  const SourceManager& source_manager = rewriter.getSourceMgr();
  auto fileID = source_manager.getMainFileID();
  const RewriteBuffer& buffer = rewriter.getEditBuffer(fileID);

  // Output to stdout
  llvm::outs() << "Transformed code:\n";
  buffer.write(llvm::outs());
  llvm::outs() << "\n";

  // Output to file
  const FileEntry* entry = source_manager.getFileEntryForID(fileID);
  std::string new_filename = RefactoredFileName(entry->tryGetRealPathName());

  std::error_code code;
  llvm::raw_fd_ostream ostream(new_filename, code, llvm::sys::fs::OF_None);
  if (code) {
    llvm::errs() << "Error: Could not open output file: " << code.message()
                 << "\n";
    return;
  }

  llvm::outs() << "Writing to file: " << new_filename << "\n";
  buffer.write(ostream);
  ostream.close();
}
