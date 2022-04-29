#include <llvm/Support/CommandLine.h>
#include <llvm/Support/TargetSelect.h>

#include "parse/Parser.hh"
#include "ast/ASTContext.hh"

#include "codegen/IrGenerator.hh"

#include <llvm/Support/FormatVariadic.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Target/TargetMachine.h>

#include <llvm/IR/LegacyPassManager.h>

class TestVisitor : public ccxx::RecursiveASTVisitor<TestVisitor> {
  public:
     bool VisitValueDecl(const ccxx::ValueDecl* D) {
         llvm::outs() << llvm::formatv("let {0}\n", D->getName());
         return true;
     }

     bool VisitBinaryOperatorExpr(const ccxx::BinaryOperatorExpr *binOp) {
         llvm::outs() << ccxx::op::operatorSpelling(binOp->getOperatorKind()) << "\n";
         return true;
     }

     bool VisitIntegerLiteralExpr(const ccxx::IntegerLiteralExpr* E) {
         llvm::outs() << E->getValue() << "\n";
         return true;
     }

     bool VisitValueTypeDecl(const ccxx::ValueTypeDecl* D) {
         llvm::outs() << ":";
         TraverseStmt(D->expr());
         return true;
     }

     bool VisitValueInitializerDecl(const ccxx::ValueInitializerDecl* D) {
         llvm::outs() << "=";
         TraverseStmt(D->expr());
         return true;
     }
};

llvm::cl::opt<std::string> inputFile(llvm::cl::Positional, llvm::cl::desc("Input file"), llvm::cl::Required);

int main(int argc, char** argv) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();

    llvm::cl::ParseCommandLineOptions(argc, argv);

    ccxx::ASTContext astContext;
    ccxx::Parser parser(astContext);

    parser.parse(inputFile.getValue());

    ccxx::IRGenerator irGenerator(astContext.getTarget());
    irGenerator.TraverseDecl(astContext.getTopLevel());

    TestVisitor testVisitor;
    testVisitor.TraverseDecl(astContext.getTopLevel());

#if _DEBUG
    irGenerator.dump();
#endif

    // just a dump of kaleidoscope object file generation example
    std::string err;
    auto target = llvm::TargetRegistry::lookupTarget(llvm::sys::getDefaultTargetTriple(), err);
    if (!target) {
        llvm::errs() << err;
        return 1;
    }

    auto CPU = "generic";
    auto Features = "";

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto TargetMachine = target->createTargetMachine(llvm::sys::getDefaultTargetTriple(), CPU, Features, opt, RM);

    irGenerator.getModule().setDataLayout(TargetMachine->createDataLayout());
    irGenerator.getModule().setTargetTriple(llvm::sys::getDefaultTargetTriple());

    auto Filename = "output.o";
    std::error_code EC;
    llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Could not open file: " << EC.message();
        return 1;
    }

    llvm::legacy::PassManager pass;
    auto FileType = llvm::CGFT_ObjectFile;

    if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return 1;
    }
    irGenerator.postTraversal();

    pass.run(irGenerator.getModule());
    dest.flush();

    // A viable executable can by linked on msvc by running:
    // link output.o /subsystem:console /out:output.exe kernel32.lib legacy_stdio_definitions.lib msvcrt.lib
    return 0;
}
