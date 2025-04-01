#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/ASTMatchers/ASTMatchers.h"

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
		std::string ClassNameToReplace,
		std::string ClassNameToInsert
	);
	
	void onEndOfTranslationUnit() override;
	void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override;
	std::string GetArgumentsFromTemplateType(const TemplateSpecializationType *TST);

private:
	ASTContext& Context;
	clang::Rewriter CodeRefactorRewriter;
	std::string ClassNameToReplace;
	std::string ClassNameToInsert;

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
		std::string ClassNameToReplace,
		std::string ClassNameToInsert
	);

	void HandleTranslationUnit(clang::ASTContext &Ctx) override;

private:
	clang::ast_matchers::MatchFinder Finder;
	CodeRefactorMatcher CodeRefactorHandler;

	std::string ClassNameToReplace;
	std::string ClassNameToInsert;
};