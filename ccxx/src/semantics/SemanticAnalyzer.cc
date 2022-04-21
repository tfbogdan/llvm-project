#include "SemanticAnalyzer.hh"

#include "ast/ASTContext.hh"
#include "type/Type.hh"

#include <llvm/ADT/BitmaskEnum.h>


namespace ccxx {

BinaryOperatorExpr *SemanticAnalyzer::createBinaryOperator(ASTContext &astContext, op::BinaryOperatorKind Op,
                                                        const Expr *lHand, const Expr *rHand) const {
    auto lQualType = lHand->resultType();
    auto rQualType = rHand->resultType();
    auto *lType = lQualType.getType();
    auto *rType = rQualType.getType();

    if (lType->isBuiltinType() && rType->isBuiltinType()) {
        const auto *lBuiltin = llvm::dyn_cast<BuiltinType>(lHand->resultType().getType());
        const auto *rBuiltin = llvm::dyn_cast<BuiltinType>(rHand->resultType().getType());

        auto lSignedness = lBuiltin->getSignedness();
        auto rSignedness = rBuiltin->getSignedness();

        Signedness resultSignedness = Signedness::Undefined;

        // No arithmetics
        assert(!lBuiltin->isVoid());
        assert(!rBuiltin->isVoid());

        BuiltinKind resultKind = BuiltinKind::Count;
        if (lBuiltin->isIntegral() && rBuiltin->isIntegral()) {
            // If both operands are integral then signed/unsigned mismatch is not allowed
            // so ensure that rule is followed
            assert(lSignedness == rSignedness);

            // Implicit signed / unsigned conversions are only allowed when the compiler can guarantee they are safe.
            // Signed to unsigned is never safe unless known at compile time and the value is positive
            // Unsigned to signed is only safe as long as the value is either known at compile time and it fits the
            //  target type or otherwise if the value is known only at runtime the conversion is only safe as long as
            //  the target signed type size is double the size of the source unsigned type
        }

        /**
         * ATM we are assuming a lot about the order of builtin declarations in BuiltinTypes.def
         * Since this is a fragile assumption it needs too be challenged by these static assertions
         * There is no double a better way to handle this is needed.
         */
        static_assert(BuiltinKind::SignedChar < BuiltinKind::SignedShort);
        static_assert(BuiltinKind::SignedShort < BuiltinKind::SignedInt);
        static_assert(BuiltinKind::SignedInt < BuiltinKind::SignedLong);
        static_assert(BuiltinKind::SignedLong < BuiltinKind::SignedLongLong);

        static_assert(BuiltinKind::UnsignedChar < BuiltinKind::UnsignedShort);
        static_assert(BuiltinKind::UnsignedShort < BuiltinKind::UnsignedInt);
        static_assert(BuiltinKind::UnsignedInt < BuiltinKind::UnsignedLong);
        static_assert(BuiltinKind::UnsignedLong < BuiltinKind::UnsignedLongLong);

        static_assert(BuiltinKind::UnsignedLongLong < BuiltinKind::Float);
        static_assert(BuiltinKind::Float < BuiltinKind::Double);

        // The result type of arithmetic operations between built in types will be the widest type of it's two operand's
        // types. Since the builtin type kinds are ordered we can simply return the largest of the two.
        resultKind = std::max(lBuiltin->getBuintinKind(), rBuiltin->getBuintinKind());
        return astContext.emplace<BinaryOperatorExpr>(Op, lHand, rHand, ValueCategory::RValue,
                                                   astContext.getBuiltinType(resultKind));
    } else {
        if (lType->isGenericType() || rType->isGenericType()) {
            // Returning generic type is simply a means to defer this analysis until we categorically know the type of
            // both operands
            return astContext.emplace<BinaryOperatorExpr>(Op, lHand, rHand, ValueCategory::RValue,
                                                       astContext.getGenericType());
        }

        // @todo user types
        // @todo pointer types
        assert(false);
        return nullptr;
    }
}

} // namespace ccxx
