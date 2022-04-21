#pragma once

namespace ccxx {

class Decl;

#define DECL(D, B) \
    class D##Decl;
#define ABSTRACT_DECL(D)
#include "ast/DeclNodes.inc"

}