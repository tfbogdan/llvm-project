#pragma once

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Support/Allocator.h>
#include <llvm/Support/Casting.h>

#include "DeclBase.hh"

namespace ccxx {

class Decl;
class NamedDecl;
class Expr;

class DeclContext {
  public:
    virtual ~DeclContext() = default;
    explicit DeclContext(DeclKind K)
        : _defKind(K) {
    }
    void addChild(Decl *d);

    static bool classof(const Decl *D) noexcept;

    /// Resolve name within this context or the parent context if not locally defined
    const NamedDecl *resolve(llvm::StringRef name);
    const NamedDecl *resolveNarrow(llvm::StringRef name);

    DeclContext *getParent();

    Decl* asDecl();

    [[deprecated]] const llvm::SmallVectorImpl<Decl *> &getChildren() const {
        return defs;
    }

    const llvm::SmallVectorImpl<Decl*> &decls() const {
        return defs;
    }

    bool isGlobalScope() const {
        return _defKind == DeclKind::TopLevel;
    }

    bool isFunction() const {
        return _defKind == DeclKind::Function;
    }

    DeclKind getDeclKind() const {
        return _defKind;
    }

  private:
    llvm::SmallVector<Decl *, 0> defs;
    llvm::StringMap<const NamedDecl *, llvm::BumpPtrAllocator> identifierLookup;
    DeclKind _defKind;
};

} // namespace ccxx
