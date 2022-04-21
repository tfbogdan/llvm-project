#pragma once

#include <cstdint>
#include <lex/TokenKinds.hh>

namespace ccxx {
namespace op {
enum class Arity : uint8_t
{
    Invalid,
    Unary,
    Binary
};

enum class Associativity : uint8_t
{
    Invalid,
    Left,
    Right
};

using Precedence = unsigned;

struct OperatorInfo {
    const Arity arity                 : 2;
    const Associativity associativity : 2;
    const Precedence precedence       : 28;
};

inline OperatorInfo opInfo(TokenKind k) {
    switch (k) {
#define BINARY_OPERATOR(Name, La, P, T)                                                                                \
    case T:                                                                                                            \
        return OperatorInfo{Arity::Binary, La ? Associativity::Left : Associativity::Right, P};
#include "OperatorInfo.def"
    default: return {};
    }
}


enum class BinaryOperatorKind : uint8_t
{
#define BINARY_OPERATOR(Name, La, P, T) Name,
#include "OperatorInfo.def"
#undef BINARY_OPERATOR
    Count
};

inline BinaryOperatorKind binOpFromTokenKind(TokenKind t) {

    switch (t) {
        //clang-format off

#define BINARY_OPERATOR(Name, La, P, T) case TokenKind::T: return BinaryOperatorKind::Name;
#include "OperatorInfo.def"
        //clang-format on

    default: return BinaryOperatorKind::Count;
    }
}

inline llvm::StringRef operatorSpelling(BinaryOperatorKind opK) {
    switch (opK) {
        //clang-format off
#define BINARY_OPERATOR(Name, La, P, T) case BinaryOperatorKind::Name: return #Name;                                                                          
#include "OperatorInfo.def"
        //clang-format on
    default: return "invalid_operator";
    }
}

} // namespace op

} // namespace ccxx
