//==============================================================================
// FILE:
//    clangpass.cpp
//
// DESCRIPTION:
//    Substitutes all input files std::atomic<T> usages with LTestAtomic<T> with
//    the same API.
//
// USAGE:
// clang++ -Xclang -load -Xclang ./build/lib/libClangPass.so -Xclang -add-plugin -Xclang ClangPass ./AtomicsReplacer/test-project/main.cpp 
//
// License: The Unlicense
//==============================================================================

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/raw_ostream.h"

#include "clangpass.h"

using namespace clang;
using namespace ast_matchers;
using namespace replace_pass;

//-----------------------------------------------------------------------------
// ASTFinder callback
//-----------------------------------------------------------------------------
CodeRefactorMatcher::CodeRefactorMatcher(
  ASTContext& Context,
  clang::Rewriter &RewriterForCodeRefactor,
  std::vector<ReplacePair> Names
) : Context(Context),
    CodeRefactorRewriter(RewriterForCodeRefactor),
    Names(Names) {}

void CodeRefactorMatcher::runFor(const ReplacePair &p, const MatchResult &Result) {
  auto type = TypeName::of(p.old_name);
  switch (type) {
    case TypeName::SimpleName: {
      if (const auto* ETL = Result.Nodes.getNodeAs<ElaboratedTypeLoc>("ElaboratedTypeLoc" + p.old_name)) {      
        CodeRefactorRewriter.ReplaceText(ETL->getSourceRange(), p.new_name);
      }
      if (const auto* QTL = Result.Nodes.getNodeAs<QualifiedTypeLoc>("QualifiedTypeLoc" + p.old_name)) {
        CodeRefactorRewriter.ReplaceText(QTL->getSourceRange(), p.new_name);
      }
    }
    case TypeName::TemplateName: {
      if (const auto* ETL = Result.Nodes.getNodeAs<ElaboratedTypeLoc>("ElaboratedTypeLoc" + p.old_name)) {      
        const auto* TemplType = ETL->getType()->getAs<TemplateSpecializationType>();
        if (!TemplType) {
          return;
        }
        CodeRefactorRewriter.ReplaceText(ETL->getSourceRange(), p.new_name + GetArgumentsFromTemplateType(TemplType));
      }
      if (const auto* QTL = Result.Nodes.getNodeAs<QualifiedTypeLoc>("QualifiedTypeLoc" + p.old_name)) {
        const auto* TemplType = QTL->getType()->getAs<TemplateSpecializationType>();
        if (!TemplType) {
          return;
        }
        CodeRefactorRewriter.ReplaceText(QTL->getSourceRange(), p.new_name + GetArgumentsFromTemplateType(TemplType));
      }
    }
  }
}

void CodeRefactorMatcher::run(const MatchResult &Result) {
  for (size_t i = 0; i < Names.size(); ++i) {
    runFor(Names[i], Result);
  }
}

std::string CodeRefactorMatcher::GetArgumentsFromTemplateType(const TemplateSpecializationType *TST) {
  std::string args;
  llvm::raw_string_ostream os(args);
  printTemplateArgumentList(os, TST->template_arguments(), Context.getPrintingPolicy());
  return args;
}

// Util function for debugging purposes
std::string CodeRefactorMatcher::getSourceRangeAsString(const SourceRange& SR) const {
  auto& sm = CodeRefactorRewriter.getSourceMgr();
  auto& langOpts = CodeRefactorRewriter.getLangOpts();

  clang::SourceLocation start = SR.getBegin();
  clang::SourceLocation end = SR.getEnd();
  end = clang::Lexer::getLocForEndOfToken(end, 0, sm, langOpts);

  bool isInvalid = false;
  const char *startData = sm.getCharacterData(start, &isInvalid);
  
  if (isInvalid) {
    return "<invalid begin>";
    isInvalid = false;
  }

  const char *endData = sm.getCharacterData(end, &isInvalid);

  if (isInvalid) {
    return "<invalid end>";
    isInvalid = false;
  }
  size_t length = endData - startData;

  return std::string(startData, length);
}


//-----------------------------------------------------------------------------
// ASTConsumer
//-----------------------------------------------------------------------------
CodeRefactorASTConsumer::CodeRefactorASTConsumer(
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

void CodeRefactorASTConsumer::HandleTranslationUnit(clang::ASTContext &Ctx) {
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

