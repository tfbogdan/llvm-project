#include "Expr.hh"
#include "ASTContext.hh"
#include "target/TargetInfo.hh"
#include "ast/DeclNodes.hh"
#include "type/Type.hh"


ccxx::IntegerLiteralExpr::IntegerLiteralExpr(const ccxx::ASTContext &astContext, llvm::APInt v)
    : Expr(StmtKind::IntegerLiteralExpr, ValueCategory::RValue, QualType(astContext.getBuiltinType(BuiltinKind::SignedInt)))
    , value(std::move(v)) {
}

ccxx::FloatingLiteralExpr::FloatingLiteralExpr(const ccxx::ASTContext &astContext, llvm::APFloat v)
    : Expr(StmtKind::FloatingLiteralExpr, ValueCategory::RValue, QualType(astContext.getBuiltinType(BuiltinKind::Double)))
    , value(std::move(v)) {
}

ccxx::StringLiteralExpr::StringLiteralExpr(const ccxx::ASTContext &astContext, llvm::StringRef v)
    : Expr(StmtKind::StringLiteralExpr, ValueCategory::RValue,
           astContext.getPointerType(
               QualType(astContext.getBuiltinType(BuiltinKind::SignedChar), TypeQualBits::tq_constBit)))
    , value(v) {
}

ccxx::CharLiteralExpr::CharLiteralExpr(const ccxx::ASTContext &astContext, int v)
    : Expr(StmtKind::CharLiteralExpr, ValueCategory::RValue,
           QualType(astContext.getBuiltinType(BuiltinKind::SignedInt), TypeQualBits::tq_constBit))
    , value(v) {
}

ccxx::DeclRefExpr::DeclRefExpr(const ValueDecl *def)
    : Expr(StmtKind::DeclRefExpr, ValueCategory::LValue, def->getValueType())
    , Def(def) {
}

ccxx::BinaryOperatorExpr::BinaryOperatorExpr(op::BinaryOperatorKind oK, const Expr *lHandTerm, const Expr *rHandTerm,
                                       ValueCategory cat, QualType type)
    : Expr(StmtKind::BinaryOperatorExpr, cat, type)
    , opKind(oK)
    , _operands{ lHandTerm, rHandTerm } {
}

ccxx::TypeExpr::TypeExpr(const ccxx::ASTContext &astContext, QualType resolvedTye)
    : Expr(StmtKind::TypeExpr, ValueCategory::RValue, QualType(astContext.getTypeRefType()))
    , _resolvedType(resolvedTye) {
}