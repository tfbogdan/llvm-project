#pragma once

#include <ranges>
#include <string>

#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/StringRef.h>

#include "DeclBase.hh"
#include "DeclContext.hh"

#include "type/Type.hh"

namespace ccxx {
class Expr;
class TypeExpr;
class BuiltinType;

class NamedDecl : public Decl {
  public:
    NamedDecl(llvm::StringRef name, DeclContext *P, DeclKind dk)
        : Decl(P, dk)
        , _name(name) {
    }

    llvm::StringRef getName() const {
        return _name;
    }

    static bool classof(const Decl *D) noexcept {
        return D->getKind() >= DeclKind::firstNamed && D->getKind() <= DeclKind::lastNamed;
    }

  private:
    llvm::SmallString<0> _name;
};

class TopLevelDecl
    : public Decl
    , public DeclContext {
  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::TopLevel;
    }

    TopLevelDecl()
        : Decl(nullptr, DeclKind::TopLevel)
        , DeclContext(DeclKind::TopLevel) {
    }
};

class ValueDecl
    : public NamedDecl
    , public DeclContext {
    QualType _Type;
    Expr *_initExpr = nullptr;
    TypeExpr *_typeExpr = nullptr;

  protected:
    ValueDecl(QualType type, llvm::StringRef name, DeclContext *P, DeclKind K, Expr *initExp, TypeExpr *typeExpr)
        : NamedDecl(name, P, K)
        , DeclContext(K)
        , _Type(type)
        , _initExpr(initExp)
        , _typeExpr(typeExpr) {
    }

  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() >= DeclKind::firstValue && D->getKind() <= DeclKind::lastValue;
    }


    ValueDecl(QualType type, llvm::StringRef name, DeclContext *P, Expr *initExp, TypeExpr *typeExpr)
        : ValueDecl(type, name, P, DeclKind::Value, initExp, typeExpr) {
    }


    inline QualType getValueType() const {
        return _Type;
    }

    Expr *getInitExpression() const {
        return _initExpr;
    }

    TypeExpr *getTypeExpr() const {
        return _typeExpr;
    }
};

class ParamDecl : public ValueDecl {
  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::Param;
    }

    ParamDecl(uint16_t paramIx, QualType type, llvm::StringRef name, DeclContext *P, Expr *initExp, TypeExpr *typeExpr)
        : ValueDecl(type, name, P, DeclKind::Param, initExp, typeExpr)
        , _paramIx(paramIx) {
    }

    uint16_t getParamIndex() const {
        return _paramIx;
    }

  private:
    uint16_t _paramIx;
};

class FunctionDecl
    : public NamedDecl
    , public DeclContext {
  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::Function;
    }

    FunctionDecl(llvm::StringRef name, DeclContext *P, bool Extern)
        : NamedDecl(name, P, DeclKind::Function)
        , DeclContext(DeclKind::Function)
        , _isExtern( Extern) {
    }

    void setParams(const std::ranges::output_range<ParamDecl *> auto &args) {
        _args.assign(args.begin(), args.end());
    }

    const ParamDecl *getParam(uint16_t ix) const {
        return _args[ix];
    }

    void setReturnTypeExpr(TypeExpr *returnTypeExpe) {
        _returnTypeExpr = returnTypeExpe;
    }

    void setBodyExpr(Expr *bodyExpr) {
        _bodyExpr = bodyExpr;
    }

    const llvm::SmallVectorImpl<ParamDecl *> &getParams() const {
        return _args;
    }

    const TypeExpr *getReturnTypeExpr() const {
        return _returnTypeExpr;
    }

    const Expr *getBody() const {
        return _bodyExpr;
    }

    bool isExtern() const {
        return _isExtern;
    }

  private:
    llvm::SmallVector<ParamDecl *, 0> _args;
    TypeExpr *_returnTypeExpr = nullptr;
    Expr *_bodyExpr = nullptr;
    bool _isExtern : 1 = false;
};

class ExprDecl : public Decl {
  protected:
    ExprDecl(DeclContext *P, DeclKind K, Expr *E)
        : Decl(P, K)
        , _expr(E) {
    }

  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() >= DeclKind::firstExpr && D->getKind() <= DeclKind::lastExpr;
    }

    const Expr *expr() const {
        return _expr;
    }

  private:
    Expr *_expr = nullptr;
};

class ValueTypeDecl : public ExprDecl {
  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::ValueType;
    }

    ValueTypeDecl(DeclContext *P, Expr *E)
        : ExprDecl(P, DeclKind::ValueType, E) {
    }
};

class ValueInitializerDecl : public ExprDecl {
  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::ValueInitializer;
    }
    ValueInitializerDecl(DeclContext *P, Expr *E)
        : ExprDecl(P, DeclKind::ValueInitializer, E) {
    }
};

class ReturnTypeDecl : public ExprDecl {
  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::ReturnType;
    }
    ReturnTypeDecl(FunctionDecl *P, Expr *E)
        : ExprDecl(P, DeclKind::ReturnType, E) {
    }
};

class BlockDecl : public Decl, public DeclContext {
    // Not a ExprDecl because a block is a CompoundStmt

  public:
    static bool classof(const Decl *D) noexcept {
        return D->getKind() == DeclKind::Block;
    }

//    BlockDecl(FunctionDecl *P, Expr *E)
//        : ExprDecl(P, DeclKind::Block, E) {
//    }
};


} // namespace ccxx
