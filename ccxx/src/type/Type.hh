#pragma once

#include <llvm/Support/Casting.h>
#include <string_view>

#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/PointerIntPair.h>
#include <llvm/ADT/PointerUnion.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Support/PointerLikeTypeTraits.h>

#include <parse/OperatorInfo.hh>

namespace ccxx {
class Type;

enum class FundamentalType : int8_t
{
#define FUNDAMENTAL_TYPE(name) ft_##name,
#include "lex/TokenKinds.def"
#undef FUNDAMENTAL_TYPE
    ft_count,
    ft_invalid
};

inline FundamentalType getFundamentalTypeFromToken(TokenKind k) {
    switch (k) {
#define FUNDAMENTAL_TYPE(name)                                                                                         \
    case TokenKind::tok_##name:                                                                                        \
        return FundamentalType::ft_##name;
#include "lex/TokenKinds.def"
#undef FUNDAMENTAL_TYPE
    default: return FundamentalType::ft_invalid;
    }
}

enum class Signedness : int8_t
{
    Undefined,
    Signed,
    Unsigned
};

struct TypeModifiers {
    Signedness Sign = Signedness::Undefined;
    bool Short = false;
    bool Long = false;
    bool LongLong = false;
};

enum class TypeQualifiers : int8_t
{
#define TYPE_QUALIFIER(name) tq_##name,
#include "lex/TokenKinds.def"
#undef TYPE_QUALIFIER
    tq_count,
    tq_invalid
};

enum TypeQualBits : uint8_t
{
    tq_noQualsBit
#define TYPE_QUALIFIER(name) , tq_##name##Bit = 1u << static_cast<unsigned>(TypeQualifiers::tq_##name)
#include "lex/TokenKinds.def"
#undef TYPE_QUALIFIER
};

class QualType {
  public:
    QualType() = default;

    QualType(const ccxx::Type *t, unsigned Quals)
        : storage(t, Quals) {
    }

    QualType(const ccxx::Type *t)
        : QualType(t, 0) {
    }

    const ccxx::Type *getType() const {
        return storage.getPointer();
    }

    bool isConst() const {
        return storage.getInt() & tq_constBit;
    }

    uintptr_t rawStorage() const {
        return reinterpret_cast<uintptr_t>(storage.getOpaqueValue());
    }

    bool operator==(QualType rhs);

  private:
    llvm::PointerIntPair<const ccxx::Type *, 2> storage;
};


class FunctionDecl;

enum : unsigned
{
    TypeAlignmentBits = 4,
    TypeAlignment = 1 << TypeAlignmentBits
};

/// \class Type
/// In CCXX, a type is:
/// - A set of values. Every values in the set counts as an instance of the type
/// - A set of operations allowed on values of that type
///
///
class Type {
  public:
    enum class TypeKind
    {
        Builtin,            // one of bool, char, int, etc
        Pointer,            // a pointer to any addressable type
        TypeReference,      // first class type
        NamespaceReference, // first class namespace
        Function,           // first class function
        Generic             // An unknown type. Used for type inference
    };

    TypeKind getTypeKind() const noexcept {
        return typeKind;
    }

    bool isBuiltinType() const {
        return getTypeKind() == TypeKind::Builtin;
    }

    bool isPointerType() const {
        return getTypeKind() == TypeKind::Pointer;
    }

    bool isGenericType() const noexcept {
        return typeKind == TypeKind::Generic;
    }

    virtual ~Type() = default;

  protected:
    explicit Type(TypeKind k) noexcept
        : typeKind(k) {
    }

    //        std::array<llvm::PointerUnion<int*,float*>, BinaryOperatorKind::BinOp_Count> binop_table;

  private:
    TypeKind typeKind;
    /// A naive way to approach the "set of operations" dimension
    /// Should be correct but not necessarily optimal
    //        llvm::StringMap<llvm::DenseSet<FunctionDecl*>> operations;
    /// Operation set includes:
    /// - operator definitions
    /// - constructors
    /// - methods
};

} // namespace ccxx

namespace llvm {

template <> struct PointerLikeTypeTraits<ccxx::Type *> {
    static inline void *getAsVoidPointer(ccxx::Type *P) {
        return P;
    }

    static inline ccxx::Type *getFromVoidPointer(void *P) {
        return static_cast<ccxx::Type *>(P);
    }

    static constexpr int NumLowBitsAvailable = ccxx::TypeAlignmentBits;
};
} // namespace llvm

namespace ccxx {

class PointerType : public Type {
  public:
    explicit PointerType(QualType pointeeT)
        : Type(TypeKind::Pointer)
        , pointee(pointeeT) {
    }


    QualType getPointee() const noexcept {
        return pointee;
    }

    static bool classof(const Type *T) noexcept {
        return T->getTypeKind() == TypeKind::Pointer;
    }

  private:
    QualType pointee;
};

class TypeRefType : public Type {
  public:
    TypeRefType()
        : Type(TypeKind::TypeReference) {
    }

    static bool classof(const Type *T) noexcept {
        return T->getTypeKind() == TypeKind::TypeReference;
    }
};

class GenericType : public Type {
  public:
    GenericType()
        : Type(TypeKind::Generic) {
    }

    static bool classof(const Type *T) noexcept {
        return T->getTypeKind() == TypeKind::Generic;
    }
};

class NamespaceType : public Type {
  public:
    NamespaceType()
        : Type(TypeKind::NamespaceReference) {
    }
    static bool classof(const Type *T) noexcept {
        return T->getTypeKind() == TypeKind::NamespaceReference;
    }
};
} // namespace ccxx

namespace llvm {
template <> struct DenseMapInfo<ccxx::QualType> {
    using ptrDenseMapInfo = DenseMapInfo<const ccxx::Type *>;
    static inline ccxx::QualType getEmptyKey() {
        return {ptrDenseMapInfo::getEmptyKey(), 0};
    }

    static inline ccxx::QualType getTombstoneKey() {
        return {ptrDenseMapInfo::getTombstoneKey(), 0};
    }

    static unsigned getHashValue(const ccxx::QualType &Val) {
        return ptrDenseMapInfo::getHashValue(reinterpret_cast<const ccxx::Type *>(Val.rawStorage()));
    }

    static bool isEqual(ccxx::QualType LHS, ccxx::QualType RHS) {
        return LHS == RHS;
    }
};
} // namespace llvm
