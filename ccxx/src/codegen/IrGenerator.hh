//
// Created by Bogdan on 08/04/2021.
//

#ifndef CCXX_IRGENERATOR_HH
#define CCXX_IRGENERATOR_HH

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/ADT/StringMap.h>

#include "target/TargetInfo.hh"
#include "ast/ASTVisitor.hh"

#include <memory>

namespace ccxx {

class IRGenerator : public RecursiveASTVisitor<IRGenerator> {
  public:
    explicit IRGenerator(const TargetInfo& target);

    bool VisitFunctionDecl(const FunctionDecl *);
    bool VisitValueDecl(const ValueDecl *);
    bool VisitParamDecl(const ParamDecl *);

    bool VisitBinaryOperatorExpr(const BinaryOperatorExpr *binOp);
    bool VisitIntegerLiteralExpr(const IntegerLiteralExpr *literal);
    bool VisitFloatingLiteralExpr(const FloatingLiteralExpr *literal);
    bool VisitStringLiteralExpr(const StringLiteralExpr *);
    bool VisitCharLiteralExpr(const CharLiteralExpr *);
    bool VisitDeclRefExpr(const DeclRefExpr *);
    bool VisitCallExpr(const CallExpr *fcCall);

    void dump() {
        module->dump();
    }

    llvm::Module& getModule() const {
        return *module;
    }

    void postTraversal();

  private:
    const TargetInfo& Target;

    llvm::Type* mapBuiltinType(const BuiltinType &ty);
    llvm::Type* mapPointerType(const PointerType &ty);

    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> irBuilder;
    std::unique_ptr<llvm::Module> module;
    llvm::StringMap<llvm::Value*> namedValues;
    llvm::BasicBlock* initGlobalsBB = nullptr;
    llvm::Function* funcUnderConstruction = nullptr;

    llvm::Value *output = nullptr;
};

}


#endif // CCXX_IRGENERATOR_HH
