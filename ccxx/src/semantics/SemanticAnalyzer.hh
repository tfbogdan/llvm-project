#pragma once

#include "parse/OperatorInfo.hh"
#include <ast/Expr.hh>

namespace ccxx {

class BinaryOperatorExpr;
class ASTContext;
class TargetInfo;

class SemanticAnalyzer {
  public:
    SemanticAnalyzer(const TargetInfo *hostInfo, const TargetInfo *targetInfo)
        : Host(hostInfo)
        , Target(targetInfo) {
    }


    BinaryOperatorExpr *createBinaryOperator(ASTContext &astContext, op::BinaryOperatorKind Op, const Expr *lHand,
                                                const Expr *rHand) const;

  private:
    const TargetInfo *Host = nullptr;
    const TargetInfo *Target = nullptr;
};

} // namespace ccxx
