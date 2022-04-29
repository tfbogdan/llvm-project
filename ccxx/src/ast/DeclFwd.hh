#pragma once

namespace ccxx {

class Decl;

#define DECL(D, B) \
    class D##Decl;

#include "ast/DeclNodes.inc"

}