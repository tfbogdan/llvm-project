#ifndef BUILTINTYPE_HH_INCLUDED
#define BUILTINTYPE_HH_INCLUDED

namespace ccxx {
/**
 * @brief The BuiltinKind enum might still be incomplete or even inaccurate
 */
enum class BuiltinKind : std::uint16_t
{
#define BUILTIN_TYPE(TypeName) TypeName,
#include "BuiltinTypes.def"
    Count
};

class BuiltinType : public Type {
    friend class ASTContext;

  public:

    llvm::StringRef canonicalName() const noexcept {
        switch (btKind) {
        case BuiltinKind::Void: return "void";
        case BuiltinKind::Bool: return "bool";
        case BuiltinKind::SignedChar: return "char";
        case BuiltinKind::UnsignedChar: return "unsigned char";
        case BuiltinKind::SignedShort: return "short int";
        case BuiltinKind::UnsignedShort: return "unsigned short int";
        case BuiltinKind::SignedInt: return "int";
        case BuiltinKind::UnsignedInt: return "unsigned int";
        case BuiltinKind::SignedLong: return "long int";
        case BuiltinKind::UnsignedLong: return "unsigned long int";
        case BuiltinKind::SignedLongLong: return "long long int";
        case BuiltinKind::UnsignedLongLong: return "unsigned long long int";
        case BuiltinKind::Float: return "float";
        case BuiltinKind::Double: return "double";
        case BuiltinKind::Count: return "";
        }
    }

    bool isIntegral() const {
        switch (btKind) {
#define INTEGER_TYPE(TypeName) case BuiltinKind::TypeName: return true;
#include "BuiltinTypes.def"
        default: return false;
        }
    }

    bool isVoid() const {
        return btKind == BuiltinKind::Void;
    }

    bool isFloating() const {
        switch (btKind) {
#define FLOATING_POINT_TYPE(TypeName) case BuiltinKind::TypeName: return true;
#include "BuiltinTypes.def"
        default: return false;
        }
    }

    Signedness getSignedness() const {
        switch (btKind) {
#define SIGNED_INTEGER_TYPE(TypeName) case BuiltinKind::TypeName: return Signedness::Signed;
#define UNSIGNED_INTEGER_TYPE(TypeName) case BuiltinKind::TypeName: return Signedness::Unsigned;
#include "BuiltinTypes.def"
        default: return Signedness::Undefined;
        }
    }

    BuiltinKind getBuintinKind() const {
        return btKind;
    }

    static bool classof(const Type *T) noexcept {
        return T->getTypeKind() == TypeKind::Builtin;
    }

  private:
    explicit BuiltinType(BuiltinKind k) noexcept
        : Type(TypeKind::Builtin)
        , btKind(k) {
    }

    BuiltinKind btKind;
};

} // namespace ccxx

#endif // BUILTINTYPE_HH_INCLUDED