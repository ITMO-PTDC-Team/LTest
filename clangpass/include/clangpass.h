#pragma once

#include <clang/AST/DeclBase.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/TypeLoc.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/ASTMatchersInternal.h>

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"

using namespace clang;
using namespace ast_matchers;
using namespace llvm;

namespace replace_pass {

struct TypeName {
  enum : int { TemplateName, SimpleName };

  static int of(std::string_view name) {
    if (name == "::std::mutex" || name == "::std::shared_mutex" ||
        name == "::std::condition_variable") {
      return TypeName::SimpleName;
    } else if (name == "::std::atomic") {
      return TypeName::TemplateName;
    } else {
      assert(false);
    }
  }
};

struct ReplacePair {
  std::string old_name;
  std::string new_name;
};

//-----------------------------------------------------------------------------
// ASTFinder callback
//-----------------------------------------------------------------------------
class CodeRefactorMatcher
    : public clang::ast_matchers::MatchFinder::MatchCallback {
 public:
  using MatchResult = clang::ast_matchers::MatchFinder::MatchResult;

  explicit CodeRefactorMatcher(ASTContext &context, clang::Rewriter &rewriter,
                               std::vector<ReplacePair> names);

  void run(const MatchResult &result) override;
  std::string GetArgumentsFromTemplateType(
      const TemplateSpecializationType *type);

 private:
  void runFor(const ReplacePair &p, const MatchResult &result);

 private:
  ASTContext &context;
  clang::Rewriter &rewriter;
  std::vector<ReplacePair> names;
};

//-----------------------------------------------------------------------------
// ASTConsumer
//-----------------------------------------------------------------------------
class CodeRefactorASTConsumer : public clang::ASTConsumer {
 public:
  CodeRefactorASTConsumer(ASTContext &context, clang::Rewriter &rewriter,
                          std::vector<ReplacePair> names,
                          std::string temp_prefix);

  void HandleTranslationUnit(clang::ASTContext &ctx) override;

 private:
  std::string RefactoredFileName(StringRef original_filename) const;

 private:
  clang::ast_matchers::MatchFinder match_finder;
  CodeRefactorMatcher refactor_handler;
  clang::Rewriter &rewriter;

  std::vector<ReplacePair> names;
  std::string temp_prefix;
};

class NameMatcherFactory {
  using ClangMatcher =
      ::clang::ast_matchers::internal::BindableMatcher<TypeLoc>;

 public:
  NameMatcherFactory() {}

  // Does not support matching the parameters of the functions
  ClangMatcher CreateMatcherFor(std::string_view name) {
    switch (TypeName::of(name)) {
      case TypeName::SimpleName: {
        return CreateSimpleMatcherFor(name);
      }
      case TypeName::TemplateName: {
        return CreateTemplateMatcherFor(name);
      }
      default:
        assert(false && "Unsupported type name");
    };
  }

  ClangMatcher CreateSimpleMatcherFor(std::string_view name) {
    return elaboratedTypeLoc(hasNamedTypeLoc(
        internal::BindableMatcher<TypeLoc>(new internal::TypeLocTypeMatcher(
            (hasDeclaration(cxxRecordDecl(hasName(name))))))));
  }

  ClangMatcher CreateTemplateMatcherFor(std::string_view name) {
    return elaboratedTypeLoc(hasNamedTypeLoc(loc(templateSpecializationType(
        hasDeclaration(classTemplateSpecializationDecl(hasName(name)))))));
  }
};
}  // namespace replace_pass