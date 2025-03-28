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


//-----------------------------------------------------------------------------
// ASTFinder callback
//-----------------------------------------------------------------------------
class CodeRefactorMatcher : public clang::ast_matchers::MatchFinder::MatchCallback {
public:
	explicit CodeRefactorMatcher(
		ASTContext& Context,
		clang::Rewriter &RewriterForCodeRefactor,
		std::vector<std::string> NamesToReplace,
		std::vector<std::string> NamesToInsert
	);
	
	void onEndOfTranslationUnit() override;
	void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override;
	std::string GetArgumentsFromTemplateType(const TemplateSpecializationType *TST);

private:
	ASTContext& Context;
	clang::Rewriter CodeRefactorRewriter;
  std::vector<std::string> NamesToReplace;
  std::vector<std::string> NamesToInsert;

	std::string getSourceRangeAsString(const SourceRange& SR) const;
};

//-----------------------------------------------------------------------------
// ASTConsumer
//-----------------------------------------------------------------------------
class CodeRefactorASTConsumer : public clang::ASTConsumer {
public:
	CodeRefactorASTConsumer(
		ASTContext& Context,
		clang::Rewriter &R,
		std::vector<std::string> NamesToReplace,
		std::vector<std::string> NamesToInsert
	);

	void HandleTranslationUnit(clang::ASTContext &Ctx) override;

private:
	clang::ast_matchers::MatchFinder Finder;
	CodeRefactorMatcher CodeRefactorHandler;

	std::vector<std::string> NamesToReplace;
	std::vector<std::string> NamesToInsert;
};

struct TypeName {
  
  enum : int {
    TemplateName, SimpleName
  };

  static int of(std::string name) {
    if (name == "::std::mutex"
      || name == "::std::shared_mutex") {
      return TypeName::SimpleName;
    } else if (name == "::std::atomic") {
      return TypeName::TemplateName;
    } else {
      assert(false);
    }
  }
};


class NameMatcherFactory {
	using ClangMatcher = ::clang::ast_matchers::internal::BindableMatcher<TypeLoc> ;

public:
	NameMatcherFactory() {

	}

  // Does not support matching the parameters of the functions
	ClangMatcher CreateMatcherFor(std::string name) {
    switch (TypeName::of(name)) {
      case TypeName::SimpleName: {
        return CreateSimpleMatcherFor(name);
      }
      case TypeName::TemplateName: {
        return CreateTemplateMatcherFor(name);
      }
    };
	}

  ClangMatcher CreateSimpleMatcherFor(std::string name) {
    return elaboratedTypeLoc(
      hasNamedTypeLoc(internal::BindableMatcher<TypeLoc>(
        new internal::TypeLocTypeMatcher((
        hasDeclaration(
          cxxRecordDecl(
            hasName(name))
        )
      ))))
    );
  }

	ClangMatcher CreateTemplateMatcherFor(std::string name) {
    return elaboratedTypeLoc(
      hasNamedTypeLoc(
        loc(
          templateSpecializationType(
          hasDeclaration(
            classTemplateSpecializationDecl(
            hasName(name)
              )
            )
          )
        )
      )
    );
	}
};