#include "ASTContext.hh"

#include "DeclNodes.hh"
#include "Expr.hh"
#include <parse/OperatorInfo.hh>
#include <target/TargetInfo.hh>
#include <type/Type.hh>
#include "semantics/SemanticAnalyzer.hh"
#include <llvm/ADT/Triple.h>

#include <llvm/Support/Host.h>

ccxx::ASTContext::~ASTContext() {}

ccxx::ASTContext::ASTContext()
    : nodeAlloc()
    , _builtinTypes{
#define BUILTIN_TYPE(TypeName) emplace<BuiltinType>(BuiltinKind::TypeName),
#include <type/BuiltinTypes.def>
      }
    , _typeRefType(emplace<TypeRefType>())
    , _genericType(emplace<GenericType>()) {

    initTargets();
    Sema = emplace<SemanticAnalyzer>(Host.get(), Target.get());

    topLevel = emplace<TopLevelDecl>();

    pushContext(topLevel);
    initBuiltins();
}

void ccxx::ASTContext::initBuiltins() {
    // @todo
}

const ccxx::Type *ccxx::ASTContext::getPointerType(QualType qT) const {
    auto p = pointerTypes.FindAndConstruct(qT);

    if (!p.second) {
        auto storage = nodeAlloc.Allocate(sizeof(PointerType), TypeAlignment);
        p.second = new (storage) PointerType(qT);
    }
    return p.second;
}

void ccxx::ASTContext::popContext() {
    assert(currentContext);
    currentContext = currentContext->getParent();
}

void ccxx::ASTContext::initTargets() {
    auto hostTripleStr = llvm::sys::getDefaultTargetTriple();

    llvm::Triple hostTriple(hostTripleStr);

    Host = TargetInfo::CreateTargetInfo();
    Target = TargetInfo::CreateTargetInfo();
}
