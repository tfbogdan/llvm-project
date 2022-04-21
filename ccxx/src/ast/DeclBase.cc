#include "DeclBase.hh"

#include "DeclNodes.hh"

namespace ccxx {
#define DECL(Name, Base)                                                                                               \
    Name##Decl *Decl::as##Name##Decl() {                                                                               \
        assert(llvm::isa<Name##Decl>(this));                                                                           \
        return static_cast<Name##Decl*>(this);                                                                          \
    }                                                                                                                  \
    const Name##Decl *Decl::as##Name##Decl() const {                                                                   \
        assert(llvm::isa<Name##Decl>(this));                                                                           \
        return static_cast<const Name##Decl*>(this);                                                                          \
    }
#define ABSTRACT_DECL(D)
#include "ast/DeclNodes.inc"

} // namespace ccxx