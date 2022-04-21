#pragma once

#include <llvm/Support/Casting.h>

#include <parse/OperatorInfo.hh>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/BitmaskEnum.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/StringRef.h>

#include "ast/Stmt.hh"
#include "type/Type.hh"

namespace ccxx {

class Type;
class ASTContext;
class ValueDecl;
class FunctionDecl;

enum class ValueCategory : int8_t
{
    LValue,
    RValue
};

class Expr : public ValueStmt {
  public:
    QualType resultType() const {
        return _type;
    }

    virtual ~Expr() = default;

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() >= StmtKind::firstExpr && S->getStmtClass() <= StmtKind::lastExpr;
    }

  protected:
    Expr(StmtKind K, ValueCategory cat, QualType type)
        : ValueStmt(K)
        , _type(type)
        , _cat(cat) {
    }

  private:
    QualType _type;
    ValueCategory _cat;
};

class DeclRefExpr : public Expr {
  public:
    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::DeclRefExpr;
    }

    DeclRefExpr(const ValueDecl *def);

    const ValueDecl *getReferencedDefinition() const {
        return Def;
    }

  private:
    const ValueDecl *Def;
};

class IntegerLiteralExpr : public Expr {
  public:
    IntegerLiteralExpr(const ccxx::ASTContext &astContext, llvm::APInt v);

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::IntegerLiteralExpr;
    }

    llvm::APInt getValue() const {
        return value;
    }

  private:
    llvm::APInt value;
};

class FloatingLiteralExpr : public Expr {
  public:
    FloatingLiteralExpr(const ccxx::ASTContext &astContext, llvm::APFloat v);
    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::FloatingLiteralExpr;
    }

    llvm::APFloat getValue() const {
        return value;
    }

  private:
    llvm::APFloat value;
};

class StringLiteralExpr : public Expr {
  public:
    StringLiteralExpr(const ccxx::ASTContext &astContext, llvm::StringRef v);

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::StringLiteralExpr;
    }

    llvm::StringRef getValue() const {
        return value;
    }

  private:
    llvm::SmallString<0> value;
};

class CharLiteralExpr : public Expr {
  public:
    CharLiteralExpr(const ccxx::ASTContext &astContext, int C);

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::CharLiteralExpr;
    }

    int getValue() const {
        return value;
    }

  private:
    int value;
};

class BinaryOperatorExpr : public Expr {
  public:
    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::BinaryOperatorExpr;
    }

    BinaryOperatorExpr(op::BinaryOperatorKind oK, const Expr *lHandTerm, const Expr *rHandTerm, ValueCategory cat,
                    QualType type);

    op::BinaryOperatorKind getOperatorKind() const noexcept {
        return opKind;
    }

    const Expr *getLHand() const {
        return _operands[0];
    }

    const Expr *getRHand() const {
        return _operands[1];
    }

    auto children() const {
        return _operands;
    }

  private:
    op::BinaryOperatorKind opKind;
    std::array<const Expr *, 2> _operands = {};
};

class CallExpr : public Expr {
  public:
    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::CallExpr;
    }

    CallExpr(const FunctionDecl *callee, ValueCategory cat, QualType resultType,
                     std::ranges::output_range<const Expr *> auto &args)
        : Expr(StmtKind::CallExpr, cat, resultType)
        , _callee(callee)
        , _args(std::begin(args), std::end(args)) {
    }

    const llvm::SmallVectorImpl<const Expr *> &getArgs() const {
        return _args;
    }

    const FunctionDecl *getCallee() const {
        return _callee;
    }

  private:
    const FunctionDecl *_callee;
    llvm::SmallVector<const Expr *, 0> _args;
};

/// An expression that resolves to a type. Eg: "int"
class TypeExpr : public Expr {
  public:
    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::TypeExpr;
    }

    TypeExpr(const ccxx::ASTContext &astContext, QualType resolvedTye);

    inline QualType getResolvedType() const {
        return _resolvedType;
    }

  private:
    QualType _resolvedType;
};

} // namespace ccxx
