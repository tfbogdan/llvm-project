#pragma once

#include <span>
#include <ranges>

namespace ccxx {

enum class StmtKind : int8_t
{
    NoStmtClass,
#define ABSTRACT_STMT(D)
#define STMT(CLASS, BASE) CLASS,
#define STMT_RANGE(BASE, START, END) first##BASE = START, last##BASE = END,
#define LAST_STMT_RANGE(BASE, START, END) first##BASE = START, last##BASE = END
#include "ast/StmtNodes.inc"
};

class alignas(void *) Stmt {
  public:
    using child_range = std::span<Stmt *>;

    explicit Stmt(StmtKind K)
        : _kind(K) {
    }

    StmtKind getStmtClass() const {
        return _kind;
    }

    child_range children() const {
        return {};
    }

  private:
    StmtKind _kind;
};

class ValueStmt : public Stmt {
  public:
    using Stmt::Stmt;

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() >= StmtKind::firstValueStmt && S->getStmtClass() <= StmtKind::lastValueStmt;
    }
};

class DeclStmt : public Stmt {
  public:
    using Stmt::Stmt;

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::DeclStmt;
    }
};

class CompoundStmt : public Stmt {
  public:
    CompoundStmt(const std::ranges::output_range<const Stmt*> auto &Stmts)
        : Stmt(StmtKind::CompoundStmt)
        , _stmts(Stmts.begin(), Stmts.end()) {
    }

    static bool classof(const Stmt *S) noexcept {
        return S->getStmtClass() == StmtKind::CompoundStmt;
    }

    const auto &children() const {
        return _stmts;
    }

  private:
    llvm::SmallVector<const Stmt *, 0> _stmts;
};

} // namespace ccxx
