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

//-----------------------------------------------------------------------------
// ASTFinder callback
//-----------------------------------------------------------------------------
CodeRefactorMatcher::CodeRefactorMatcher(
  ASTContext& Context,
  clang::Rewriter &RewriterForCodeRefactor,
  std::vector<std::string> NamesToReplace,
  std::vector<std::string> NamesToInsert
) : Context(Context),
    CodeRefactorRewriter(RewriterForCodeRefactor),
    NamesToReplace(NamesToReplace),
    NamesToInsert(NamesToInsert) {}

void CodeRefactorMatcher::onEndOfTranslationUnit() {
  const SourceManager& SM = CodeRefactorRewriter.getSourceMgr();    
  FileID MainFileID = SM.getMainFileID();
  const RewriteBuffer& Buffer = CodeRefactorRewriter.getEditBuffer(MainFileID);

  // Output to stdout
  llvm::outs() << "Transformed code:\n";
  Buffer.write(llvm::outs());
  llvm::outs() << "\n";

  // Output to file
  const FileEntry *Entry = SM.getFileEntryForID(MainFileID);
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

void CodeRefactorMatcher::run(const clang::ast_matchers::MatchFinder::MatchResult &Result) {
  for (size_t i = 0; i < NamesToReplace.size(); ++i) {
    std::string replaceName = NamesToReplace[i];
    std::string insertName = NamesToInsert[i];
    if (const auto* ETL = Result.Nodes.getNodeAs<ElaboratedTypeLoc>("ElaboratedTypeLoc" + replaceName)) {      
      const auto* TemplType = ETL->getType()->getAs<TemplateSpecializationType>();
      if (!TemplType) {
        break;
      }
        CodeRefactorRewriter.ReplaceText(ETL->getSourceRange(), insertName + GetArgumentsFromTemplateType(TemplType));
    }

    if (const auto* QTL = Result.Nodes.getNodeAs<QualifiedTypeLoc>("QualifiedTypeLoc" + replaceName)) {
      const auto* TemplType = QTL->getType()->getAs<TemplateSpecializationType>();
      if (!TemplType) {
        break;
      }
      CodeRefactorRewriter.ReplaceText(QTL->getSourceRange(), insertName + GetArgumentsFromTemplateType(TemplType));
    }
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
  std::vector<std::string> NamesToReplace,
  std::vector<std::string> ClassNameToInsert
): CodeRefactorHandler(Context, R, NamesToReplace, ClassNameToInsert),
    NamesToReplace(NamesToReplace),
    NamesToInsert(ClassNameToInsert) {

  auto factory = NameMatcherFactory();
  
  for (auto name: NamesToReplace) {
    const auto MatcherForFQTemplateTypes = factory.CreateMatcherFor(name);


    // Uses previous matcher inside, but returns a wrapping QualifiedTypeLoc node
    // which is used in the function parameters
    const auto MatcherForFQTemplateParams = qualifiedTypeLoc(
      hasUnqualifiedLoc(
        MatcherForFQTemplateTypes
      )
    );
    Finder.addMatcher(MatcherForFQTemplateTypes.bind("ElaboratedTypeLoc" + name), &CodeRefactorHandler);
    Finder.addMatcher(MatcherForFQTemplateParams.bind("QualifiedTypeLoc" + name), &CodeRefactorHandler);
  }
}

void CodeRefactorASTConsumer::HandleTranslationUnit(clang::ASTContext &Ctx) {
  Finder.matchAST(Ctx);
}

