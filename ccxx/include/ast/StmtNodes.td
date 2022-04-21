include "ASTNode.td"

class StmtNode<StmtNode base, bit abstract = 0> : ASTNode {
  StmtNode Base = base;
  bit Abstract = abstract;
}

def Stmt        : StmtNode<?, 1>;
def DeclStmt    : StmtNode<Stmt>;

def ValueStmt   : StmtNode<Stmt, 1>;
    def Expr : StmtNode<ValueStmt, 1>;

        def DeclRefExpr : StmtNode<Expr>;
        def IntegerLiteralExpr : StmtNode<Expr>;
        def FloatingLiteralExpr : StmtNode<Expr>;
        def StringLiteralExpr : StmtNode<Expr>;
        def CharLiteralExpr : StmtNode<Expr>;
        def BinaryOperatorExpr : StmtNode<Expr>;
        def CallExpr : StmtNode<Expr>;

        def TypeExpr : StmtNode<Expr>;

def CompoundStmt : StmtNode<Stmt>;
