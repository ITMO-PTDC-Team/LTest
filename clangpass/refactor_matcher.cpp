#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clangpass.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace ast_matchers;
using namespace replace_pass;

//-----------------------------------------------------------------------------
// ASTFinder callback
//-----------------------------------------------------------------------------
CodeRefactorMatcher::CodeRefactorMatcher(ASTContext& context,
                                         clang::Rewriter& rewriter,
                                         std::vector<ReplacePair> names)
    : context(context), rewriter(rewriter), names(names) {}

void CodeRefactorMatcher::runFor(const ReplacePair& p,
                                 const MatchResult& result) {
  auto type = TypeName::of(p.old_name);
  switch (type) {
    case TypeName::SimpleName: {
      if (const auto* ETL = result.Nodes.getNodeAs<ElaboratedTypeLoc>(
              "ElaboratedTypeLoc" + p.old_name)) {
        rewriter.ReplaceText(ETL->getSourceRange(), p.new_name);
      }
      if (const auto* QTL = result.Nodes.getNodeAs<QualifiedTypeLoc>(
              "QualifiedTypeLoc" + p.old_name)) {
        rewriter.ReplaceText(QTL->getSourceRange(), p.new_name);
      }
    }
    case TypeName::TemplateName: {
      if (const auto* ETL = result.Nodes.getNodeAs<ElaboratedTypeLoc>(
              "ElaboratedTypeLoc" + p.old_name)) {
        const auto* TemplType =
            ETL->getType()->getAs<TemplateSpecializationType>();
        if (!TemplType) {
          return;
        }
        rewriter.ReplaceText(
            ETL->getSourceRange(),
            p.new_name + GetArgumentsFromTemplateType(TemplType));
      }
      if (const auto* QTL = result.Nodes.getNodeAs<QualifiedTypeLoc>(
              "QualifiedTypeLoc" + p.old_name)) {
        const auto* TemplType =
            QTL->getType()->getAs<TemplateSpecializationType>();
        if (!TemplType) {
          return;
        }
        rewriter.ReplaceText(
            QTL->getSourceRange(),
            p.new_name + GetArgumentsFromTemplateType(TemplType));
      }
    }
  }
}

void CodeRefactorMatcher::run(const MatchResult& result) {
  for (size_t i = 0; i < names.size(); ++i) {
    runFor(names[i], result);
  }
}

std::string CodeRefactorMatcher::GetArgumentsFromTemplateType(
    const TemplateSpecializationType* type) {
  std::string args;
  llvm::raw_string_ostream os(args);
  printTemplateArgumentList(os, type->template_arguments(),
                            context.getPrintingPolicy());
  return args;
}
