#pragma once

#include <cstdint>
#include <llvm/ADT/StringRef.h>

namespace ccxx {
enum TokenKind : uint16_t
{
#define TOK(tok) tok,
#include <lex/TokenKinds.def>
    Token_Count
};
#undef TOK

llvm::StringRef tokenSpelling(TokenKind tk);

inline bool isTypeModifier(ccxx::TokenKind t) {
    switch (t) {
#define TYPE_MODIFIER(modf)                                                                                            \
    case ccxx::TokenKind::tok_##modf:                                                                                  \
        return true;
#include "TokenKinds.def"
#undef TYPE_MODIFIER
    default: return false;
    }
}

inline bool isFundamentalType(ccxx::TokenKind t) {
    switch (t) {
#define FUNDAMENTAL_TYPE(ftname)                                                                                       \
    case ccxx::TokenKind::tok_##ftname:                                                                                \
        return true;
#include "TokenKinds.def"
#undef FUNDAMENTAL_TYPE
    default: return false;
    }
}

inline bool isTypeQualifier(ccxx::TokenKind t) {
    switch (t) {
#define TYPE_QUALIFIER(name)                                                                                           \
    case ccxx::TokenKind::tok_##name:                                                                                  \
        return true;
#include "TokenKinds.def"
#undef TYPE_QUALIFIER
    default: return false;
    }
}

} // namespace ccxx
