#include "clang/AST/ASTConsumer.h"
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
