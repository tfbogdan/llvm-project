#pragma once

#include <array>
#include <memory>
#include <utility>
#include <vector>

#include <type/Type.hh>
#include <type/BuiltinType.hh>

#include <llvm/ADT/DenseMap.h>
#include <llvm/Support/Allocator.h>

namespace ccxx {

class Decl;
class Expr;
class TopLevelDecl;
class DeclContext;
class TargetInfo;
class SemanticAnalyzer;

class ASTContext {
  public:
    ASTContext();
    ~ASTContext();

    template <typename T, typename... ArgsT> T *emplace(ArgsT &&...args) {
        void *mem = nodeAlloc.Allocate(sizeof(T), 16);
        new (mem) T(std::forward<ArgsT>(args)...);
        return static_cast<T *>(mem);
    }

    const BuiltinType *getBuiltinType(BuiltinKind k) const {
        return _builtinTypes[static_cast<size_t>(k)];
    }

    TopLevelDecl *getTopLevel() const {
        return topLevel;
    }

    DeclContext *getCurrentContext() const {
        return currentContext;
    }

    void pushContext(DeclContext *C) {
        currentContext = C;
    }

    void popContext();

    const Type *getPointerType(QualType q) const;
    const TypeRefType* getTypeRefType() const {
        return _typeRefType;
    }

    const GenericType* getGenericType() const {
        return _genericType;
    }

    const SemanticAnalyzer& getSema() const {
        return *Sema;
    }

    const TargetInfo& getTarget() const {
        return *Target;
    }

  private:
    void initTargets();
    void initBuiltins();

    TopLevelDecl *topLevel = nullptr;
    DeclContext *currentContext = nullptr;
    mutable llvm::BumpPtrAllocator nodeAlloc;
    mutable llvm::DenseMap<QualType, const Type *> pointerTypes;

    std::unique_ptr<const TargetInfo> Host = nullptr;
    std::unique_ptr<const TargetInfo> Target = nullptr;

    SemanticAnalyzer* Sema = nullptr;

    std::array<const BuiltinType *, static_cast<size_t>(BuiltinKind::Count)> _builtinTypes;
    const TypeRefType* _typeRefType = nullptr;
    const GenericType* _genericType = nullptr;
};
} // namespace ccxx
