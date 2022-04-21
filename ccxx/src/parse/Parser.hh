#pragma once

#include <filesystem>

#include <lex/Lexer.hh>

#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>

#include <ast/DeclFwd.hh>

namespace ccxx {
class ASTContext;
class Expr;

class BuiltinType;

class TypeExpr;

class Parser {
    using DiagKind = llvm::SourceMgr::DiagKind;

  public:
    explicit Parser(ASTContext &astC);
    void parse(const std::filesystem::path &);

  private:
    NamedDecl *parseDefinition();
    FunctionDecl *parseFunctionDef();
    ParamDecl *parseParamDef(uint16_t pix);
    // NamespaceDef *parseNamespaceDef();
    ValueDecl *parseValueDef();

    const BuiltinType *parseBuiltinType();

    Expr *parseExpression();

    /// Parses an expression where a type is expected.
    /// Needs to be a different function since type expressions are simplified expressions. eg: no operators are allowed
    TypeExpr *parseTypeExpression();

    Lexer lexer;
    ASTContext &astContext;
    llvm::SourceMgr sourceMgr;
};

} // namespace ccxx
