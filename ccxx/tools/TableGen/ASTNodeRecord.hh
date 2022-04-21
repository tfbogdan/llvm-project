#ifndef CCXX_AST_NODE_RECORD_INCLUDED
#define CCXX_AST_NODE_RECORD_INCLUDED

#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"

#include <map>
#include <string_view>
#include <vector>
#include <set>

namespace ccxx {

class ASTNode {
  public:
    ASTNode(llvm::Record *record = nullptr)
        : _record(record) {

    }

    ASTNode getBase() const {
        return _record->getValueAsOptionalDef("Base");
    }

    bool isAbstract() const {
        return _record->getValueAsBit("Abstract");
    }

    llvm::StringRef getName() const {
        return _record ? _record->getName() : "";
    }

    llvm::ArrayRef<llvm::SMLoc> getLoc() const {
        return _record ? _record->getLoc() : llvm::ArrayRef<llvm::SMLoc>{};
    }

    bool operator<(const ASTNode &rhs) const {
        return getName() < rhs.getName();
    }

    bool operator==(const ASTNode &rhs) const {
        return getName() == rhs.getName();
    }

    explicit operator bool() const {
        return _record != nullptr;
    }

  private:
    llvm::Record *_record = nullptr;
};

class ASTNodesGenerator {
  private:
    std::multimap<ASTNode, ASTNode> Tree;
    llvm::RecordKeeper &Records;
    std::string NodeClassName;
    std::string BaseSuffix;
    ASTNode Root;
    std::string MacroHierarchyName;

  public:
    ASTNodesGenerator(llvm::RecordKeeper &keeper, std::string nodeClassName, std::string baseSuffix = "")
        : Records(keeper)
        , NodeClassName(std::move(nodeClassName))
        , BaseSuffix(std::move(baseSuffix)) {

        // Emit statements
        const std::vector<llvm::Record *> Stmts = Records.getAllDerivedDefinitions(NodeClassName);

        for (unsigned i = 0, e = Stmts.size(); i != e; ++i) {
            llvm::Record *R = Stmts[i];

            if (auto B = R->getValueAsOptionalDef("Base")) {
                Tree.insert(std::make_pair(B, R));
            } else if (Root) {
                PrintFatalError(R->getLoc(), llvm::Twine("multiple root nodes in \"") + "Base" + "\" hierarchy");
            } else
                Root = R;
        }

        if (!Root)
            PrintFatalError(llvm::Twine("didn't find root node in \"") + "Base" + "\" hierarchy");
    }

    // Create a macro-ized version of a name
    static std::string macroName(std::string S) {
        for (unsigned i = 0; i < S.size(); ++i)
            S[i] = std::toupper(S[i]);

        return S;
    }

    // Return the name to be printed in the base field. Normally this is
    // the record's name plus the base suffix, but if it is the root node and
    // the suffix is non-empty, it's just the suffix.
    std::string baseName(ASTNode node) {
        if (node == Root && !BaseSuffix.empty())
            return BaseSuffix;

        return node.getName().str() + BaseSuffix;
    }

    const std::string &macroHierarchyName() {
        assert(Root && "root node not yet derived!");
        if (MacroHierarchyName.empty())
            MacroHierarchyName = macroName(std::string(Root.getName()));
        return MacroHierarchyName;
    }

    std::pair<ASTNode, ASTNode> generateNode(ASTNode Base, llvm::raw_ostream &OS) {
        std::string BaseName = macroName(std::string(Base.getName()));

        auto i = Tree.lower_bound(Base), e = Tree.upper_bound(Base);
        bool HasChildren = (i != e);

        ASTNode First, Last;
        if (!Base.isAbstract())
            First = Last = Base;

        for (; i != e; ++i) {
            ASTNode Child = i->second;
            bool Abstract = Child.isAbstract();
            std::string NodeName = macroName(std::string(Child.getName()));

            OS << "#ifndef " << NodeName << "\n";
            OS << "#  define " << NodeName << "(Type, Base) " << BaseName << "(Type, Base)\n";
            OS << "#endif\n";

            if (Abstract)
                OS << "ABSTRACT_" << macroHierarchyName() << "(";
            OS << NodeName << "(" << Child.getName() << ", " << baseName(Base) << ")";
            if (Abstract)
                OS << ")";
            OS << "\n";

            auto Result = generateNode(Child, OS);
            assert(Result.first && Result.second && "node didn't have children?");

            // Update the range of Base.
            if (!First)
                First = Result.first;
            Last = Result.second;

            OS << "#undef " << NodeName << "\n\n";
        }
        // If there aren't first/last nodes, it must be because there were no
        // children and this node was abstract, which is not a sensible combination.
        if (!First) {
            PrintFatalError(Base.getLoc(), "abstract node has no children");
        }
        assert(Last && "set First without Last");

        if (HasChildren) {
            // Use FOO_RANGE unless this is the last of the ranges, in which case
            // use LAST_FOO_RANGE.
            if (Base == Root)
                OS << "LAST_" << macroHierarchyName() << "_RANGE(";
            else
                OS << macroHierarchyName() << "_RANGE(";
            OS << Base.getName() << ", " << First.getName() << ", "
               << Last.getName() << ")\n\n";
        }

        return std::make_pair(First, Last);

    }

    void generate(llvm::raw_ostream &OS) {
        llvm::emitSourceFileHeader("List of AST nodes of a particular kind", OS);

        OS << "// This is a generated file, do not edit by hand!\n";
        // Write the preamble
        OS << "#ifndef ABSTRACT_" << macroHierarchyName() << "\n";
        OS << "#  define ABSTRACT_" << macroHierarchyName() << "(Type) Type\n";
        OS << "#endif\n";

        OS << "#ifndef " << macroHierarchyName() << "_RANGE\n";
        OS << "#  define "
           << macroHierarchyName() << "_RANGE(Base, First, Last)\n";
        OS << "#endif\n\n";

        OS << "#ifndef LAST_" << macroHierarchyName() << "_RANGE\n";
        OS << "#  define LAST_"
           << macroHierarchyName() << "_RANGE(Base, First, Last) "
           << macroHierarchyName() << "_RANGE(Base, First, Last)\n";
        OS << "#endif\n\n";

        generateNode(Root, OS);

        OS << "#undef " << macroHierarchyName() << "\n";
        OS << "#undef " << macroHierarchyName() << "_RANGE\n";
        OS << "#undef LAST_" << macroHierarchyName() << "_RANGE\n";
        OS << "#undef ABSTRACT_" << macroHierarchyName() << "\n";
    }
};

inline void EmitDeclContext(llvm::RecordKeeper &Records, llvm::raw_ostream &OS) {
    // Copied from clang source base as is
    // FIXME: Find a .td file format to allow for this to be represented better.
    llvm::emitSourceFileHeader("List of AST Decl nodes", OS);

    OS << "#ifndef DECL_CONTEXT\n";
    OS << "#  define DECL_CONTEXT(DECL)\n";
    OS << "#endif\n";

    OS << "#ifndef DECL_CONTEXT_BASE\n";
    OS << "#  define DECL_CONTEXT_BASE(DECL) DECL_CONTEXT(DECL)\n";
    OS << "#endif\n";

    typedef std::set<llvm::Record*> RecordSet;
    typedef std::vector<llvm::Record*> RecordVector;

    RecordVector DeclContextsVector
        = Records.getAllDerivedDefinitions("DeclContext");
    RecordVector Decls = Records.getAllDerivedDefinitions("DeclNode");
    RecordSet DeclContexts (DeclContextsVector.begin(), DeclContextsVector.end());

    for (RecordVector::iterator i = Decls.begin(), e = Decls.end(); i != e; ++i) {
        llvm::Record *R = *i;

        if (llvm::Record *B = R->getValueAsOptionalDef("Base")) {
            if (DeclContexts.find(B) != DeclContexts.end()) {
                OS << "DECL_CONTEXT_BASE(" << B->getName() << ")\n";
                DeclContexts.erase(B);
            }
        }
    }

    // To keep identical order, RecordVector may be used
    // instead of RecordSet.
    for (RecordVector::iterator
             i = DeclContextsVector.begin(), e = DeclContextsVector.end();
         i != e; ++i)
        if (DeclContexts.find(*i) != DeclContexts.end())
            OS << "DECL_CONTEXT(" << (*i)->getName() << ")\n";

    OS << "#undef DECL_CONTEXT\n";
    OS << "#undef DECL_CONTEXT_BASE\n";
}

} // namespace ccxx

#endif // CCXX_AST_NODE_RECORD_INCLUDED