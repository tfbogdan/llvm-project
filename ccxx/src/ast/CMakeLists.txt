target_sources(ccxx PRIVATE
        ASTContext.cc
        ASTContext.hh
        AST.cc
        AST.hh
        DeclNodes.hh
        DeclNodes.cc
        Expr.cc
        Expr.hh
        DeclContext.cc
        DeclContext.hh
        ASTVisitor.hh
        DeclBase.hh
        DeclBase.cc
        Stmt.cc
        Stmt.hh
        )

target_precompile_headers(ccxx PRIVATE
        ASTContext.hh
        AST.hh
        DeclNodes.hh
        DeclFwd.hh
        Expr.hh
        DeclContext.hh
        Stmt.hh
        ASTVisitor.hh
        DeclBase.hh
        Stmt.hh
        )