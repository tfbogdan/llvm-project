#pragma once

#include "ast/DeclNodes.hh"
#include "ast/Expr.hh"

#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Compiler.h>

namespace ccxx {

// A helper macro to implement short-circuiting when recursing.  It
// invokes CALL_EXPR, which must be a method call, on the derived
// object (s.t. a user of RecursiveASTVisitor can override the method
// in CALL_EXPR).
#define TRY_TO(CALL_EXPR)                                                                                              \
    do {                                                                                                               \
        if (!getDerived().CALL_EXPR)                                                                                   \
            return false;                                                                                              \
    } while (false)

namespace detail {

template <typename T, typename U> struct has_same_member_pointer_type : std::false_type {};
template <typename T, typename U, typename R, typename... P>
struct has_same_member_pointer_type<R (T::*)(P...), R (U::*)(P...)> : std::true_type {};

template <bool has_same_type> struct is_same_method_impl {
    template <typename FirstMethodPtrTy, typename SecondMethodPtrTy>
    static bool isSameMethod(FirstMethodPtrTy FirstMethodPtr, SecondMethodPtrTy SecondMethodPtr) {
        return false;
    }
};

template <> struct is_same_method_impl<true> {
    template <typename FirstMethodPtrTy, typename SecondMethodPtrTy>
    static bool isSameMethod(FirstMethodPtrTy FirstMethodPtr, SecondMethodPtrTy SecondMethodPtr) {
        return FirstMethodPtr == SecondMethodPtr;
    }
};

/// Returns true if and only if \p FirstMethodPtr and \p SecondMethodPtr
/// are pointers to the same non-static member function.
template <typename FirstMethodPtrTy, typename SecondMethodPtrTy>
bool isSameMethod(FirstMethodPtrTy FirstMethodPtr, SecondMethodPtrTy SecondMethodPtr) {
    return is_same_method_impl<has_same_member_pointer_type<FirstMethodPtrTy, SecondMethodPtrTy>::value>::isSameMethod(
        FirstMethodPtr, SecondMethodPtr);
}

} // end namespace detail


template <typename Derived> class RecursiveASTVisitor {

  public:
    /// A queue used for performing data recursion over statements.
    /// Parameters involving this type are used to implement data
    /// recursion over Stmts and Exprs within this class, and should
    /// typically not be explicitly specified by derived classes.
    /// The bool bit indicates whether the statement has been traversed or not.
    using DataRecursionQueue = llvm::SmallVectorImpl<llvm::PointerIntPair<const Stmt *, 1, bool>>;

    /// Return a reference to the derived class.
    Derived &getDerived() {
        return *static_cast<Derived *>(this);
    }

    /// Return whether this visitor should traverse post-order.
    bool shouldTraversePostOrder() const {
        return false;
    }

    /// Return whether this visitor should recurse into implicit
    /// code, e.g., implicit constructors and destructors.
    bool shouldVisitImplicitCode() const {
        return false;
    }


    /// Recursively visit a statement or expression, by
    /// dispatching to Traverse*() based on the argument's dynamic type.
    ///
    /// \returns false if the visitation was terminated early, true
    /// otherwise (including when the argument is nullptr).
    bool TraverseStmt(const Stmt *S, DataRecursionQueue *Queue = nullptr);

    /// Invoked before visiting a statement or expression via data recursion.
    ///
    /// \returns false to skip visiting the node, true otherwise.
    bool dataTraverseStmtPre(const Stmt *S) {
        return true;
    }

    /// Invoked after visiting a statement or expression via data recursion.
    /// This is not invoked if the previously invoked \c dataTraverseStmtPre
    /// returned false.
    ///
    /// \returns false if the visitation was terminated early, true otherwise.
    bool dataTraverseStmtPost(const Stmt *S) {
        return true;
    }

    /// Recursively visit a declaration, by dispatching to
    /// Traverse*Decl() based on the argument's dynamic type.
    ///
    /// \returns false if the visitation was terminated early, true
    /// otherwise (including when the argument is NULL).
    bool TraverseDecl(const Decl *D);

  private:
    // Traverse the given statement. If the most-derived traverse function takes a
    // data recursion queue, pass it on; otherwise, discard it. Note that the
    // first branch of this conditional must compile whether or not the derived
    // class can take a queue, so if we're taking the second arm, make the first
    // arm call our function rather than the derived class version.
#define TRAVERSE_STMT_BASE(NAME, CLASS, VAR, QUEUE)                                                                    \
    (::ccxx::detail::has_same_member_pointer_type<decltype(&RecursiveASTVisitor::Traverse##NAME),                      \
                                                  decltype(&Derived::Traverse##NAME)>::value                           \
         ? static_cast<std::conditional_t<                                                                             \
               ::ccxx::detail::has_same_member_pointer_type<decltype(&RecursiveASTVisitor::Traverse##NAME),            \
                                                            decltype(&Derived::Traverse##NAME)>::value,                \
               Derived &, RecursiveASTVisitor &>>(*this)                                                               \
               .Traverse##NAME(static_cast<const CLASS *>(VAR), QUEUE)                                                 \
         : getDerived().Traverse##NAME(static_cast<const CLASS *>(VAR)))

// Try to traverse the given statement, or enqueue it if we're performing data
// recursion in the middle of traversing another statement. Can only be called
// from within a DEF_TRAVERSE_STMT body or similar context.
#define TRY_TO_TRAVERSE_OR_ENQUEUE_STMT(S)                                                                             \
    do {                                                                                                               \
        if (!TRAVERSE_STMT_BASE(Stmt, Stmt, S, Queue))                                                                 \
            return false;                                                                                              \
    } while (false)

  public:
// Declare Traverse*() for all concrete Stmt classes.
#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, PARENT) bool Traverse##CLASS(const CLASS *S, DataRecursionQueue *Queue = nullptr);
#include "ast/StmtNodes.inc"
    // The above header #undefs ABSTRACT_STMT and STMT upon exit.

    // Define WalkUpFrom*() and empty Visit*() for all Stmt classes.
    bool WalkUpFromStmt(const Stmt *S) {
        return getDerived().VisitStmt(S);
    }
    bool VisitStmt(const Stmt *S) {
        return true;
    }

#define STMT(CLASS, PARENT)                                                                                            \
    bool WalkUpFrom##CLASS(const CLASS *S) {                                                                           \
        TRY_TO(WalkUpFrom##PARENT(S));                                                                                 \
        TRY_TO(Visit##CLASS(S));                                                                                       \
        return true;                                                                                                   \
    }                                                                                                                  \
    bool Visit##CLASS(const CLASS *S) {                                                                                \
        return true;                                                                                                   \
    }
#include "ast/StmtNodes.inc"

    bool dataTraverseNode(const Stmt *S, DataRecursionQueue *Queue);

// ---- Methods on Decls ----

// Declare Traverse*() for all concrete Decl classes.
#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE) bool Traverse##CLASS##Decl(const CLASS##Decl *D);
#include "ast/DeclNodes.inc"

    // Define WalkUpFrom*() and empty Visit*() for all Decl classes.
    bool WalkUpFromDecl(const Decl *D) {
        return getDerived().VisitDecl(D);
    }
    bool VisitDecl(const Decl *D) {
        return true;
    }

#define DECL(CLASS, BASE)                                                                                              \
    bool WalkUpFrom##CLASS##Decl(const CLASS##Decl *D) {                                                               \
        TRY_TO(WalkUpFrom##BASE(D));                                                                                   \
        TRY_TO(Visit##CLASS##Decl(D));                                                                                 \
        return true;                                                                                                   \
    }                                                                                                                  \
    bool Visit##CLASS##Decl(const CLASS##Decl *D) {                                                                    \
        return true;                                                                                                   \
    }
#include "ast/DeclNodes.inc"

  private:
    bool PostVisitStmt(const Stmt *S);
    bool TraverseDeclContextHelper(const DeclContext *DC);
    bool canIgnoreChildDeclWhileTraversingDeclContext(const Decl *Child);
};

template <typename Derived>
bool RecursiveASTVisitor<Derived>::dataTraverseNode(const Stmt *S, DataRecursionQueue *Queue) {
    // Top switch stmt: dispatch to TraverseFooStmt for each concrete FooStmt.
    switch (S->getStmtClass()) {
    case StmtKind::NoStmtClass: break;
#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, PARENT)                                                                                            \
    case StmtKind::CLASS: return TRAVERSE_STMT_BASE(CLASS, CLASS, S, Queue);
        // clang-format off
#include "ast/StmtNodes.inc"
        // clang-format on
    }

    return true;
}

template <typename Derived> bool RecursiveASTVisitor<Derived>::PostVisitStmt(const Stmt *S) {
    // In pre-order traversal mode, each Traverse##STMT method is responsible for
    // calling WalkUpFrom. Therefore, if the user overrides Traverse##STMT and
    // does not call the default implementation, the WalkUpFrom callback is not
    // called. Post-order traversal mode should provide the same behavior
    // regarding method overrides.
    //
    // In post-order traversal mode the Traverse##STMT method, when it receives a
    // DataRecursionQueue, can't call WalkUpFrom after traversing children because
    // it only enqueues the children and does not traverse them. TraverseStmt
    // traverses the enqueued children, and we call WalkUpFrom here.
    //
    // However, to make pre-order and post-order modes identical with regards to
    // whether they call WalkUpFrom at all, we call WalkUpFrom if and only if the
    // user did not override the Traverse##STMT method. We implement the override
    // check with isSameMethod calls below.

    switch (S->getStmtClass()) {
    case StmtKind::NoStmtClass: break;
#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, PARENT)                                                                                            \
    case StmtKind::CLASS:                                                                                              \
        if (::ccxx::detail::isSameMethod(&RecursiveASTVisitor::Traverse##CLASS, &Derived::Traverse##CLASS)) {          \
            TRY_TO(WalkUpFrom##CLASS(static_cast<const CLASS *>(S)));                                                  \
        }                                                                                                              \
        break;
#include "ast/StmtNodes.inc"
    }

    return true;
}

template <typename Derived> bool RecursiveASTVisitor<Derived>::TraverseStmt(const Stmt *S, DataRecursionQueue *Queue) {
    if (!S)
        return true;

    if (Queue) {
        Queue->push_back({S, false});
        return true;
    }

    llvm::SmallVector<llvm::PointerIntPair<const Stmt *, 1, bool>, 8> LocalQueue;
    LocalQueue.push_back({S, false});

    while (!LocalQueue.empty()) {
        auto &CurrSAndVisited = LocalQueue.back();
        const Stmt *CurrS = CurrSAndVisited.getPointer();
        bool Visited = CurrSAndVisited.getInt();
        if (Visited) {
            LocalQueue.pop_back();
            TRY_TO(dataTraverseStmtPost(CurrS));
            if (getDerived().shouldTraversePostOrder()) {
                TRY_TO(PostVisitStmt(CurrS));
            }
            continue;
        }

        if (getDerived().dataTraverseStmtPre(CurrS)) {
            CurrSAndVisited.setInt(true);
            size_t N = LocalQueue.size();
            TRY_TO(dataTraverseNode(CurrS, &LocalQueue));
            // Process new children in the order they were added.
            std::reverse(LocalQueue.begin() + N, LocalQueue.end());
        } else {
            LocalQueue.pop_back();
        }
    }

    return true;
}

template <typename Derived> bool RecursiveASTVisitor<Derived>::TraverseDecl(const Decl *D) {
    if (!D)
        return true;

    // As a syntax visitor, by default we want to ignore declarations for
    // implicit declarations (ones not typed explicitly by the user).
    if (!getDerived().shouldVisitImplicitCode() && D->isImplicit())
        return true;

    switch (D->getKind()) {
#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)                                                                                              \
    case DeclKind::CLASS:                                                                                              \
        if (!getDerived().Traverse##CLASS##Decl(static_cast<const CLASS##Decl *>(D)))                                  \
            return false;                                                                                              \
        break;
#include "ast/DeclNodes.inc"
    }
    return true;
}

// ----------------- Decl traversal -----------------
//
// For a Decl, we automate (in the DEF_TRAVERSE_DECL macro) traversing
// the children that come from the DeclContext associated with it.
// Therefore each Traverse* only needs to worry about children other
// than those.

template <typename Derived>
bool RecursiveASTVisitor<Derived>::canIgnoreChildDeclWhileTraversingDeclContext(const Decl *Child) {
    // @todo: We don't have any of these but we will

    //    // BlockDecls are traversed through BlockExprs,
    //    // CapturedDecls are traversed through CapturedStmts.
    //    if (isa<BlockDecl>(Child) || isa<CapturedDecl>(Child))
    //        return true;
    //    // Lambda classes are traversed through LambdaExprs.
    //    if (const CXXRecordDecl* Cls = dyn_cast<CXXRecordDecl>(Child))
    //        return Cls->isLambda();
    return false;
}

template <typename Derived> bool RecursiveASTVisitor<Derived>::TraverseDeclContextHelper(const DeclContext *DC) {
    if (!DC)
        return true;

    for (const auto *Child : DC->decls()) {
        if (!canIgnoreChildDeclWhileTraversingDeclContext(Child))
            TRY_TO(TraverseDecl(Child));
    }

    return true;
}

// This macro makes available a variable D, the passed-in decl.
#define DEF_TRAVERSE_DECL(DECL)                                                                                        \
    template <typename Derived> bool RecursiveASTVisitor<Derived>::Traverse##DECL(const DECL *D) {                     \
        bool ShouldVisitChildren = true;                                                                               \
        bool ReturnValue = true;                                                                                       \
        if (!getDerived().shouldTraversePostOrder())                                                                   \
            TRY_TO(WalkUpFrom##DECL(D));                                                                               \
        if (ReturnValue && ShouldVisitChildren)                                                                        \
            TRY_TO(TraverseDeclContextHelper(llvm::dyn_cast<const DeclContext>(D)));                                   \
        if (ReturnValue && getDerived().shouldTraversePostOrder())                                                     \
            TRY_TO(WalkUpFrom##DECL(D));                                                                               \
        return ReturnValue;                                                                                            \
    }

#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE) DEF_TRAVERSE_DECL(CLASS##Decl)
#include "ast/DeclNodes.inc"
#undef DEF_TRAVERSE_DECL

// ----------------- Stmt traversal -----------------
//
// For stmts, we automate (in the DEF_TRAVERSE_STMT macro) iterating
// over the children defined in children() (every stmt defines these,
// though sometimes the range is empty).  Each individual Traverse*
// method only needs to worry about children other than those.  To see
// what children() does for a given class, see, e.g.,
//   http://clang.llvm.org/doxygen/Stmt_8cpp_source.html

// This macro makes available a variable S, the passed-in stmt.
#define DEF_TRAVERSE_STMT(STMT)                                                                                        \
    template <typename Derived>                                                                                        \
    bool RecursiveASTVisitor<Derived>::Traverse##STMT(const STMT *S, DataRecursionQueue *Queue) {                      \
        bool ShouldVisitChildren = true;                                                                               \
        bool ReturnValue = true;                                                                                       \
        if (!getDerived().shouldTraversePostOrder())                                                                   \
            TRY_TO(WalkUpFrom##STMT(S));                                                                               \
        if (ShouldVisitChildren) {                                                                                     \
            for (const Stmt *SubStmt : S->children()) {                                                                \
                TRY_TO_TRAVERSE_OR_ENQUEUE_STMT(SubStmt);                                                              \
            }                                                                                                          \
        }                                                                                                              \
        /* Call WalkUpFrom if TRY_TO_TRAVERSE_OR_ENQUEUE_STMT has traversed the                                        \
         * children already. If TRY_TO_TRAVERSE_OR_ENQUEUE_STMT only enqueued the                                      \
         * children, PostVisitStmt will call WalkUpFrom after we are done visiting                                     \
         * children. */                                                                                                \
        if (!Queue && ReturnValue && getDerived().shouldTraversePostOrder()) {                                         \
            TRY_TO(WalkUpFrom##STMT(S));                                                                               \
        }                                                                                                              \
        return ReturnValue;                                                                                            \
    }

#define ABSTRACT_STMT(S)
#define STMT(CLASS, PARENT) DEF_TRAVERSE_STMT(CLASS)
#include <ast/StmtNodes.inc>
} // namespace ccxx
