#include <llvm/Support/CommandLine.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/Signals.h>
#include <llvm/TableGen/Error.h>
#include <llvm/TableGen/Main.h>
#include <llvm/TableGen/Record.h>

#include "ASTNodeRecord.hh"

using namespace llvm;
enum ActionType
{
    PrintRecords,
    GenDeclNodes,
    GenStmtNodes
};

namespace {

cl::opt<ActionType>
    Action(cl::desc("Action to perform:"),
           cl::values(clEnumValN(PrintRecords, "print-records", "Print all records to stdout (default)"),
                      clEnumValN(GenDeclNodes, "gen-decl-nodes", "Generates all decl nodes"),
                      clEnumValN(GenStmtNodes, "gen-stmt-nodes", "Generates all stmt nodes")));

bool CCXXTableGenMain(raw_ostream &OS, RecordKeeper &Records) {
    switch (Action.getValue()) {
    case PrintRecords:
        OS << Records; // No argument, dump all contents
        break;
    case GenDeclNodes: {
        ccxx::ASTNodesGenerator generator(Records, "DeclNode", "Decl");
        generator.generate(OS);
        ccxx::EmitDeclContext(Records, OS);
    } break;
    case GenStmtNodes: {
        ccxx::ASTNodesGenerator generator(Records, "StmtNode", "");
        generator.generate(OS);
    };
    }

    return false;
}
} // namespace

int main(int argc, char **argv) {
    llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
    llvm::PrettyStackTraceProgram X(argc, argv);
    llvm::cl::ParseCommandLineOptions(argc, argv);

    llvm::llvm_shutdown_obj Y;

    return llvm::TableGenMain(argv[0], &CCXXTableGenMain);
}