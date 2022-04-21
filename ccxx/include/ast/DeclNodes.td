include "ASTNode.td"

class DeclNode<DeclNode base, bit abstract = 0> : ASTNode {
  DeclNode Base = base;
  bit Abstract = abstract;
}

class DeclContext;

def Decl    : DeclNode<?, 1>;
def TopLevel : DeclNode<Decl>, DeclContext;

def Named   : DeclNode<Decl, 1>;
    def Value   : DeclNode<Named>, DeclContext;
        def Param : DeclNode<Value>;

    def Function : DeclNode<Named>, DeclContext;

def Expr    : DeclNode<Decl, 1>;
    def ValueType : DeclNode<Expr>;
    def ValueInitializer : DeclNode<Expr>;
    def ReturnType : DeclNode<Expr>;

def Block : DeclNode<Decl>, DeclContext;