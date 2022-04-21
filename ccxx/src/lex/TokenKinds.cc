#include "TokenKinds.hh"


llvm::StringRef ccxx::tokenSpelling(ccxx::TokenKind tk) {

    switch (tk) {
#define TOK(t)                                                                                                         \
    case t: return #t;
#include "TokenKinds.def"
#undef TOK
    case Token_Count: return {};
    }
    return {};
}
