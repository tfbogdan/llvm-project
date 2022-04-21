#include "DeclContext.hh"
#include "DeclNodes.hh"

void ccxx::DeclContext::addChild(Decl *d) {
    defs.push_back(d);
    if (auto named = llvm::dyn_cast<NamedDecl>(d); named) {
        identifierLookup.insert_or_assign(named->getName(), named);
    }
}

bool ccxx::DeclContext::classof(const Decl *D) noexcept {
    switch (D->getKind()) {
#define DECL(CLASS, BASE)
#define DECL_CONTEXT(CLASS)                                                                                            \
    case DeclKind::CLASS: return true;
// clang-format off
#include "ast/DeclNodes.inc"
        // clang-format on
    default:
#define DECL(CLASS, BASE)
#define DECL_CONTEXT_BASE(CLASS)                                                                                       \
    if (D->getKind() >= DeclKind::first##CLASS && D->getKind() <= DeclKind::last##CLASS) {                               \
        return true;                                                                                                   \
    }
// clang-format off
#include "ast/DeclNodes.inc"
        // clang-format on
        return false;
    }
}

const ccxx::NamedDecl *ccxx::DeclContext::resolve(llvm::StringRef name) {
    // First look for the name in own children
    if (auto res = identifierLookup.find(name); res != identifierLookup.end()) {
        return res->second;
    }

    DeclContext *pContext = getParent();
    if (pContext) {
        return pContext->resolve(name);
    }

    return nullptr;
}

const ccxx::NamedDecl *ccxx::DeclContext::resolveNarrow(llvm::StringRef name) {
    if (auto res = identifierLookup.find(name); res != identifierLookup.end()) {
        return res->second;
    }
    return nullptr;
}

ccxx::DeclContext *ccxx::DeclContext::getParent() {
    return asDecl()->getDeclContext();
}

ccxx::Decl *ccxx::DeclContext::asDecl() {
    switch (_defKind) {
#define DECL(CLASS, BASE)
#define DECL_CONTEXT(CLASS)                                                                                            \
    case DeclKind::CLASS: return static_cast<CLASS##Decl *>(this);
// clang-format off
#include "ast/DeclNodes.inc"
        // clang-format on
    default: return nullptr;
    }
}
