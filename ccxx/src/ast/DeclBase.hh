#pragma once

#include <cstdint>

#include "DeclFwd.hh"

namespace ccxx {

class DeclContext;

enum class DeclKind : int8_t
{
#define DECL(DERIVED, BASE) DERIVED,
#define ABSTRACT_DECL(DECL)
#define DECL_RANGE(BASE, START, END) \
        first##BASE = START, last##BASE = END,
#define LAST_DECL_RANGE(BASE, START, END) \
        first##BASE = START, last##BASE = END
#include "ast/DeclNodes.inc"
};

class Decl {
  public:

    [[nodiscard]] DeclKind getKind() const noexcept {
        return _kind;
    }

    [[nodiscard]] DeclContext *getDeclContext() const {
        return _parent;
    }

    virtual ~Decl() = default;

    bool isImplicit() const { return false; }

#define DECL(Name, Base) \
    Name##Decl* as##Name##Decl(); \
    const Name##Decl* as##Name##Decl() const;
#define ABSTRACT_DECL(D)
#include "ast/DeclNodes.inc"

  protected:
    Decl(DeclContext *P, DeclKind dk) noexcept
        : _kind(dk)
        , _parent(P) {
    }

  private:
    DeclKind _kind;
    DeclContext *_parent = nullptr;
};

}
