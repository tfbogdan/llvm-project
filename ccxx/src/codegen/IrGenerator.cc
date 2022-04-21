//
// Created by Bogdan on 08/04/2021.
//

#include "IrGenerator.hh"
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <type/BuiltinType.hh>

ccxx::IRGenerator::IRGenerator(const TargetInfo &T)
    : Target(T)
    , irBuilder(llvmContext)
    , module(std::make_unique<llvm::Module>("mod", llvmContext)) {
}

TargetInfo::IntType builtinToIntType(ccxx::BuiltinKind k) {
    switch (k) {

#define INTEGER_TYPE(TypeName)                                                                                         \
    case ccxx::BuiltinKind::TypeName: return clang::TargetInfo::IntType::TypeName;
// clang-format off
#include <type/BuiltinTypes.def>
        // clang-format on
    default: return TargetInfo::IntType::NoInt;
    }
}

namespace ccxx {

bool IRGenerator::VisitBinaryOperatorExpr(const BinaryOperatorExpr *binOp) {
    TraverseStmt(binOp->getLHand());
    llvm::Value *lHand = output;
    TraverseStmt(binOp->getRHand());
    llvm::Value *rHand = output;

    assert(binOp->getLHand()->resultType().getType()->isBuiltinType());
    assert(binOp->getRHand()->resultType().getType()->isBuiltinType());
    assert(binOp->resultType().getType()->isBuiltinType());

    auto *lType = static_cast<const BuiltinType *>(binOp->getLHand()->resultType().getType());
    auto *rType = static_cast<const BuiltinType *>(binOp->getRHand()->resultType().getType());
    auto *resType = static_cast<const BuiltinType *>(binOp->resultType().getType());

    llvm::Type *resultType = mapBuiltinType(*resType);

    auto ensureTypeConsistency = [resType, resultType, this](llvm::Value **v, const BuiltinType *ty) {
        if (ty->getBuintinKind() != resType->getBuintinKind()) {
            // we need to cast the operand to the result type
            if (resType->isIntegral()) {
                // If the result tpye is integral then both operand types must be integral. Otherwise this is a bug in
                // semantic analysis
                assert(ty->isIntegral());
                *v = irBuilder.CreateIntCast(*v, resultType, resType->getSignedness() != Signedness::Unsigned,
                                             "intcasttmp");
            } else {
                // must be floating.
                assert(resType->isFloating());
                *v = irBuilder.CreateFPCast(*v, resultType, "fpcasttmp");
            }
        }
    };

    ensureTypeConsistency(&lHand, lType);
    ensureTypeConsistency(&rHand, rType);

    switch (binOp->getOperatorKind()) {
    case op::BinaryOperatorKind::addition: output = irBuilder.CreateAdd(lHand, rHand, "addtmp"); break;
    case op::BinaryOperatorKind::multiplication: output = irBuilder.CreateMul(lHand, rHand, "multmp"); break;
    default: output = nullptr; break;
    }
    return true;
}

bool IRGenerator::VisitIntegerLiteralExpr(const IntegerLiteralExpr *literal) {
    // @todo: this will trip an assert in llvm::APInt if the value does not fit in the size of an int
    // This is just an attempt to ensure the type of the create value is int, whatever int may be on the target platform
    llvm::APInt value(Target.getTypeWidth(TargetInfo::IntType::SignedInt),
                      literal->getValue().getLimitedValue());
    output = llvm::ConstantInt::get(llvmContext, value);
    return true;
}

bool IRGenerator::VisitCharLiteralExpr(const CharLiteralExpr *literal) {
    llvm::APInt value(Target.getTypeWidth(TargetInfo::IntType::SignedInt), literal->getValue());
    output = llvm::ConstantInt::get(llvmContext, value);
    return true;
}

bool IRGenerator::VisitFloatingLiteralExpr(const FloatingLiteralExpr *literal) {
    output = llvm::ConstantFP::get(llvmContext, literal->getValue());
    return true;
}

bool IRGenerator::VisitStringLiteralExpr(const StringLiteralExpr *strLiteral) {
    /* nop now */
    output = llvm::ConstantDataArray::getString(llvmContext, strLiteral->getValue());
    // output = constDataArray;

    // output = irBuilder.CreateGlobalString(strLiteral->getValue());

    return true;
}

bool IRGenerator::VisitDeclRefExpr(const DeclRefExpr *nameRef) {
    auto *def = nameRef->getReferencedDefinition();
    switch (def->getKind()) {
    case DeclKind::Param:
        output = funcUnderConstruction->getArg(static_cast<const ParamDecl &>(*def).getParamIndex());
        break;
    case DeclKind::Value: {
        auto *valueDef = static_cast<const ValueDecl *>(def);
        if (valueDef->getDeclContext()->isGlobalScope()) {
            output = irBuilder.CreateLoad(module->getNamedGlobal(valueDef->getName()), false,
                                          llvm::formatv("load_{0}", valueDef->getName()));
        } else {
            // @todo: local variables
            assert(false);
        }
    } break;
    default: assert(false);
    }
    return true;
}


bool IRGenerator::VisitCallExpr(const CallExpr *fcCall) {
    llvm::SmallVector<llvm::Value *, 10> args;
    llvm::Function *callee = module->getFunction(fcCall->getCallee()->getName());

    for (size_t ix = 0; ix < fcCall->getArgs().size(); ++ix) {
        auto arg = fcCall->getArgs()[ix];

        TraverseStmt(arg);
        auto calleeArgTy = callee->getArg(ix)->getType();
        if (output->getType() != calleeArgTy) {
            // argument type mismatch. Eventually, these should be addressed at AST level
            // There are a few cases where this may be acceptable

            if (auto arrayType = llvm::cast<llvm::ArrayType>(output->getType());
                arrayType && calleeArgTy->isPointerTy()) {
                auto ptrType = llvm::cast<llvm::PointerType>(calleeArgTy);

                if (arrayType->getElementType() == ptrType->getPointerElementType()) {

                    auto *GV = new llvm::GlobalVariable(
                        *module, output->getType(), true, llvm::GlobalValue::PrivateLinkage,
                        llvm::cast<llvm::Constant>(output), "", nullptr, llvm::GlobalVariable::NotThreadLocal, 0);

                    GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
                    GV->setAlignment(llvm::Align(1));

                    llvm::Constant *Zero =
                        llvm::ConstantInt::get(llvm::Type::getIntNTy(llvmContext, Target.getIntWidth()), 0);
                    llvm::Constant *Indices[] = {Zero, Zero};

                    output = llvm::ConstantExpr::getInBoundsGetElementPtr(GV->getValueType(), GV, Indices);
                }
            }
        }
        args.push_back(output);
    }

    output = irBuilder.CreateCall(callee, args, "calltmp");
    return true;
}

bool IRGenerator::VisitFunctionDecl(const FunctionDecl *funcDef) {
    assert(funcDef->getReturnTypeExpr());
    assert(funcDef->getReturnTypeExpr()->getResolvedType().getType()->isBuiltinType());
    assert(funcDef->isExtern() || funcDef->getBody());
    llvm::SmallVector<llvm::Type *, 8> paramTypes;
    for (auto *param : funcDef->getParams()) {
        assert(param->getValueType().getType()->isBuiltinType() || param->getValueType().getType()->isPointerType());
        if (param->getValueType().getType()->isBuiltinType()) {
            paramTypes.push_back(mapBuiltinType(*static_cast<const BuiltinType *>(param->getValueType().getType())));
        } else if (param->getValueType().getType()->isPointerType()) {
            paramTypes.push_back(mapPointerType(*static_cast<const PointerType *>(param->getValueType().getType())));
        } else {
            assert(false); // "Only builtins and pointer types can be used atm");
        }
    }

    auto funcTy = llvm::FunctionType::get(
        mapBuiltinType(*static_cast<const BuiltinType *>(funcDef->getReturnTypeExpr()->getResolvedType().getType())),
        paramTypes, false);

    module->getOrInsertFunction(funcDef->getName(), funcTy);

    auto *func = module->getFunction(funcDef->getName());
    int16_t argIx = 0;
    for (llvm::Argument &param : func->args()) {
        param.setName(funcDef->getParam(param.getArgNo())->getName());
    }
    func->setLinkage(funcDef->getName() == "main" ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                                                  : llvm::GlobalValue::LinkageTypes::InternalLinkage);


    if (!funcDef->isExtern()) {
        auto *BB = llvm::BasicBlock::Create(llvmContext, llvm::formatv("{0}_body", funcDef->getName()), func);

        irBuilder.SetInsertPoint(BB);
        funcUnderConstruction = func;

        TraverseStmt(funcDef->getBody());
        irBuilder.CreateRet(output);
    }
    return true;
}

void IRGenerator::postTraversal() {
    auto *BB = initGlobalsBB;

    irBuilder.SetInsertPoint(BB);
    irBuilder.CreateRetVoid();
}

bool IRGenerator::VisitParamDecl(const ParamDecl *) {
    return true;
}

bool IRGenerator::VisitValueDecl(const ValueDecl *valueDecl) {
    if (llvm::isa<ParamDecl>(valueDecl)) {
        // For now, simply ignore param decls during visitation until the IR generator is refactored
        // away from the AST visitor
        return true;
    }

    assert(valueDecl->getDeclContext()->isGlobalScope());

    if (valueDecl->getDeclContext()->isGlobalScope()) {
        assert(valueDecl->getValueType().getType()->isBuiltinType());
        /*auto *global = */
        auto *resultLLVMType = mapBuiltinType(*static_cast<const BuiltinType *>(valueDecl->getValueType().getType()));
        module->getOrInsertGlobal(valueDecl->getName(), resultLLVMType);

        auto *global = module->getNamedGlobal(valueDecl->getName());
        global->setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
        global->setInitializer(llvm::ConstantInt::getIntegerValue(
            resultLLVMType,
            llvm::APInt(Target.getTypeWidth(builtinToIntType(
                            static_cast<const BuiltinType *>(valueDecl->getValueType().getType())->getBuintinKind())),
                        0)));


        // For global variables whose values are not compile time constants we need to perform initialization via a
        // runtime function

        auto *globalInitFunc = llvm::getOrCreateInitFunction(*module, "initGlobals");
        globalInitFunc->setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
        if (!initGlobalsBB) {
            initGlobalsBB = llvm::BasicBlock::Create(llvmContext, llvm::formatv("initGlobals"), globalInitFunc);
        }
        auto *BB = initGlobalsBB;

        irBuilder.SetInsertPoint(BB);

        TraverseStmt(valueDecl->getInitExpression());
        llvm::Value *valueExpr = output;

        auto *initExprLLVMType =
            mapBuiltinType(*static_cast<const BuiltinType *>(valueDecl->getInitExpression()->resultType().getType()));
        assert(valueDecl->getValueType().getType() == valueDecl->getInitExpression()->resultType().getType());

        irBuilder.CreateStore(valueExpr, global);
    }

    return true;
}

llvm::Type *IRGenerator::mapBuiltinType(const BuiltinType &ty) {
    //    switch (ty.getBuintinKind()) {
    //    case BuiltinKind::Char:
    //        // return llvm::Type::getIntNTy(Target.getCharWidth());
    //    }
    if (ty.isIntegral()) {
        auto intType = builtinToIntType(ty.getBuintinKind());
        return llvm::Type::getIntNTy(llvmContext, Target.getTypeWidth(intType));
    }

    return nullptr;
}

llvm::Type *IRGenerator::mapPointerType(const PointerType &ty) {
    auto *pointee = ty.getPointee().getType();

    switch (pointee->getTypeKind()) {
    case Type::TypeKind::Pointer: return mapPointerType(*static_cast<const PointerType *>(pointee))->getPointerTo();
    case Type::TypeKind::Builtin: return mapBuiltinType(*static_cast<const BuiltinType *>(pointee))->getPointerTo();
    default: assert(false);
    }
    return nullptr;
}

} // namespace ccxx
