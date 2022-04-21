#include "Parser.hh"

#include <cassert>
#include <fstream>
#include <sstream>
#include <stack>

#include <ast/ASTContext.hh>

#include "semantics/SemanticAnalyzer.hh"
#include <ast/DeclNodes.hh>
#include <ast/Expr.hh>

#include <llvm/ADT/IndexedMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Support/FormatVariadic.h>

#include "type/Type.hh"
#include <parse/OperatorInfo.hh>

struct OperatorTokenInfo {
    ccxx::Token tok;
    ccxx::op::OperatorInfo opInfo;
};

ccxx::Parser::Parser(ASTContext &astC)
    : astContext(astC) {
}

void ccxx::Parser::parse(const std::filesystem::path &input) {
    sourceMgr.AddNewSourceBuffer(std::move(llvm::MemoryBuffer::getFileOrSTDIN(llvm::Twine(input.string())).get()),
                                 llvm::SMLoc{});

    lexer.setBuffer(sourceMgr.getBufferInfo(1).Buffer->getBufferStart(),
                    sourceMgr.getBufferInfo(1).Buffer->getBufferEnd());
    while (!lexer.lookaheadMatch({TokenKind::eof})) {
        if (auto *def = parseDefinition(); def == nullptr) {
            auto bad_token = lexer.next();
            sourceMgr.PrintMessage(bad_token.loc, llvm::SourceMgr::DiagKind::DK_Error,
                                   llvm::formatv("Unexpected token: {0}", tokenSpelling(bad_token.kind)));
            std::exit(-1);
        } else {
            astContext.getTopLevel()->addChild(def);
        }
    }
}

ccxx::NamedDecl *ccxx::Parser::parseDefinition() {
    if (lexer.lookaheadMatch({TokenKind::tok_let, TokenKind::identifier})) {
        return parseValueDef();
    } else if (lexer.lookaheadMatch({TokenKind::tok_def })) {
        return parseFunctionDef();
    }
    /*
    if (lexer.lookaheadMatch(
            {TokenKind::identifier, TokenKind::colonequal, TokenKind::tok_var}))
    { return parseVariableDef();
    }
    if (lexer.lookaheadMatch(
            {TokenKind::identifier, TokenKind::colonequal,
    TokenKind::tok_union})) { return {};
    }
    if (lexer.lookaheadMatch(
            {TokenKind::identifier, TokenKind::colonequal,
    TokenKind::tok_union})) { return {};
    }
    if (lexer.lookaheadMatch(
            {TokenKind::identifier, TokenKind::colonequal,
    TokenKind::tok_struct})) { return {};
    }
    if (lexer.lookaheadMatch(
            {TokenKind::identifier, TokenKind::colonequal,
    TokenKind::tok_class})) { return {};
    }
    if (lexer.lookaheadMatch({TokenKind::identifier, TokenKind::colonequal,
                              TokenKind::tok_namespace})) {
      return parseNamespaceDef();
    }
    if (lexer.lookaheadMatch(
            {TokenKind::identifier, TokenKind::colonequal,
    TokenKind::tok_function})) { return parseFunctionDef();
    }
    if (lexer.lookaheadMatch({TokenKind::identifier, TokenKind::colonequal})) {
      return parseBindingDef();
    }
    */
    return {};
}

ccxx::Expr *ccxx::Parser::parseExpression() {
    const int starting_depth = lexer.getParenDepth();
    std::stack<Token> output;
    std::stack<OperatorTokenInfo> operators;

    std::stack<Expr *> outputExpressions;

    bool inExpression = true;
    while (inExpression) {
        if (lexer.lookaheadMatch({TokenKind::integral_constant})) {
            auto tok = lexer.next();
            outputExpressions.emplace(
                astContext.emplace<IntegerLiteralExpr>(astContext, std::move(std::get<llvm::APInt>(tok.V))));
            output.push(tok);
        } else if (lexer.lookaheadMatch({TokenKind::string_literal})) {
            auto tok = lexer.next();
            outputExpressions.emplace(
                astContext.emplace<StringLiteralExpr>(astContext, std::move(std::get<llvm::SmallString<0>>(tok.V))));
            output.push(tok);
        } else if (lexer.lookaheadMatch(TokenKind::char_literal) ) {
            auto tok = lexer.next();
            outputExpressions.emplace(
                astContext.emplace<CharLiteralExpr>(astContext, std::get<int>(tok.V)));
            output.push(tok);
        } else if (lexer.lookaheadMatch({TokenKind::identifier})) {
            auto tok = lexer.next();
            if (auto def = astContext.getCurrentContext()->resolve(std::get<llvm::SmallString<0>>(tok.V)); def) {
                // Determine what kind of identifier this is
                switch (def->getKind()) {
                case DeclKind::Value: [[fallthrough]];
                case DeclKind::Param:
                    outputExpressions.emplace(astContext.emplace<DeclRefExpr>(static_cast<const ValueDecl *>(def)));
                    break;
                case DeclKind::Function: {
                    auto funcDef = static_cast<const FunctionDecl *>(def);
                    if (lexer.lookaheadDiscard(TokenKind::l_paren)) {
                        // clearly a function call
                        llvm::SmallVector<const Expr *, 4> args;

                        for (auto *param : funcDef->getParams()) {
                            args.push_back(parseExpression());
                            lexer.lookaheadDiscard(TokenKind::comma);
                        }
                        lexer.skip(TokenKind::r_paren);

                        outputExpressions.push(astContext.emplace<CallExpr>(
                            funcDef, ValueCategory::RValue,
                            funcDef->getReturnTypeExpr() ? funcDef->getReturnTypeExpr()->getResolvedType()
                                                         : astContext.getGenericType(),
                            args));
                    } else {
                        // @todo still not sure how to handle function references
                        assert(false);
                    }
                } break;

                default:
                    sourceMgr.PrintMessage(tok.loc, DiagKind::DK_Error, "Don't know what to do with this identifier.");
                    exit(-1);
                }
            } else {
                sourceMgr.PrintMessage(
                    tok.loc, DiagKind::DK_Error,
                    llvm::formatv("Unknown identifier: '{0}'", std::get<llvm::SmallString<0>>(tok.V)));
                exit(-1);
            }
            output.push(tok);
        } else if (lexer.lookaheadMatch({TokenKind::l_paren})) {
            auto tok = lexer.next();
            operators.emplace(tok, op::OperatorInfo{});
        } else if (lexer.lookaheadMatch({TokenKind::r_paren})) {
            const auto lookaheadParen = lexer.lookahead(1).back();
            if (lookaheadParen.paren_depth <= starting_depth) {
                inExpression = false;
            } else {
                lexer.next(); // eat the )
                while (!operators.empty() and operators.top().tok.kind != l_paren) {
                    output.push(operators.top().tok);

                    auto opInfo = operators.top();
                    if (opInfo.opInfo.arity == op::Arity::Binary) {
                        auto *rHand = outputExpressions.top();
                        outputExpressions.pop();
                        auto *lHand = outputExpressions.top();
                        outputExpressions.pop();
                        outputExpressions.push(astContext.getSema().createBinaryOperator(
                            astContext, op::binOpFromTokenKind(opInfo.tok.kind), lHand, rHand));

                    } else {
                        // @todo unary ops
                        assert(false);
                    }

                    operators.pop();
                }
                if (operators.empty()) {
                    // mismatched paranthesis. Parse error
                }
                assert(operators.top().tok.kind == l_paren);
                operators.pop();
            }
        } else {
            // @todo: The parameter to lookahead could be a template arg and
            // result could be array
            auto lookaheadToken = lexer.lookahead(1).back();

            if (auto binOp = op::binOpFromTokenKind(lookaheadToken.kind); binOp != op::BinaryOperatorKind::Count) {
                auto tok = lexer.next();
                OperatorTokenInfo opTokInfo{tok, op::opInfo(tok.kind)};
                while ((!operators.empty()) and
                       ((operators.top().opInfo.precedence > opTokInfo.opInfo.precedence) or
                        (operators.top().opInfo.precedence == opTokInfo.opInfo.precedence and
                         opTokInfo.opInfo.associativity == op::Associativity::Left)) and
                       (operators.top().tok.kind != l_paren)) {
                    output.push(operators.top().tok);

                    auto opInfo = operators.top();
                    if (opInfo.opInfo.arity == op::Arity::Binary) {
                        auto *rHand = outputExpressions.top();
                        outputExpressions.pop();
                        auto *lHand = outputExpressions.top();
                        outputExpressions.pop();
                        outputExpressions.push(astContext.getSema().createBinaryOperator(
                            astContext, op::binOpFromTokenKind(opInfo.tok.kind), lHand, rHand));

                    } else {
                        // @todo unary ops
                        assert(false);
                    }

                    operators.pop();
                }
                switch (opTokInfo.opInfo.arity) {
                case op::Arity::Binary: break;
                case op::Arity::Unary:
                    /*no unary ops yet so no do*/ break;
                default:
                    // @todo: error
                    exit(-1);
                }
                operators.push(opTokInfo);
            } else if (ccxx::isTypeModifier(lookaheadToken.kind) || ccxx::isFundamentalType(lookaheadToken.kind)) {
                assert(false); // @todo cleanup
                // outputExpressions.push(parseTypeWithModifiers());
            } else {
                inExpression = false;
            }
        }
    }

    while (!operators.empty()) {
        output.push(operators.top().tok);
        auto opInfo = operators.top();
        if (opInfo.opInfo.arity == op::Arity::Binary) {
            auto *rHand = outputExpressions.top();
            outputExpressions.pop();
            auto *lHand = outputExpressions.top();
            outputExpressions.pop();
            outputExpressions.push(astContext.getSema().createBinaryOperator(
                astContext, op::binOpFromTokenKind(opInfo.tok.kind), lHand, rHand));

        } else {
            // @todo unary ops
            assert(false);
        }

        operators.pop();
    }
    if (outputExpressions.size() != 1) {
        sourceMgr.PrintMessage(output.top().loc, llvm::SourceMgr::DiagKind::DK_Error, "Malformed expression");
        exit(-1);
    }
    return outputExpressions.top();
}

ccxx::FunctionDecl *ccxx::Parser::parseFunctionDef() {
    lexer.skip(TokenKind::tok_def);
    const bool isExternDefinition = lexer.lookaheadDiscard(TokenKind::tok_extern);

    auto identTok = lexer.next();
    auto &functionName = std::get<llvm::SmallString<0>>(identTok.V);

    // If next token is a <, we have meta arguments
    if (lexer.lookaheadMatch({TokenKind::less})) {
        // Parse all meta arguments until none are left
        throw std::runtime_error("Not implemented");
    }

    llvm::SmallVector<ParamDecl *, 0> params;
    auto *functionDef = astContext.emplace<FunctionDecl>(functionName, astContext.getCurrentContext(), isExternDefinition);

    astContext.pushContext(functionDef);

    if (lexer.lookaheadDiscard(TokenKind::l_paren)) {
        // this allows for a trailing comma in parameter lists. whether this is good
        // or bad.. as of yet it's undefined
        uint16_t paramIx = 0;
        while (!lexer.lookaheadMatch(TokenKind::r_paren)) {
            params.push_back(parseParamDef(paramIx++));
            functionDef->addChild(params.back());
            lexer.lookaheadDiscard(TokenKind::comma);
        }
        lexer.skip(TokenKind::r_paren);
    }
    functionDef->setParams(params);

    Expr *returnTypeExpr = nullptr;
    if (lexer.lookaheadDiscard(TokenKind::arrow)) {
        functionDef->setReturnTypeExpr(parseTypeExpression());
    } else if ( isExternDefinition ) {
        sourceMgr.PrintMessage(lexer.currentLoc(), DiagKind::DK_Error, "Extern functions must always have explicit return types");
        exit(-1);
    }

    if ( !isExternDefinition ) {
        Expr *bodyExpr = nullptr;
        if (lexer.lookaheadDiscard(TokenKind::colon)) {
            functionDef->setBodyExpr(parseExpression());
            if (!lexer.lookaheadDiscard(TokenKind::semicolon)) {
                sourceMgr.PrintMessage(lexer.currentLoc(), DiagKind::DK_Error, "Expected 'semicolon' after statement");
                exit(-1);
            }

        } else if (lexer.lookaheadDiscard(TokenKind::l_brace)) {
            // @todo: parse compound statement
            lexer.eatUntilNextCurlyBrace();
        }
    } else {
        // Treat semicolon as optional after extern function declaration
        lexer.lookaheadDiscard(TokenKind::semicolon);
    }

    astContext.popContext();
    return functionDef;
}


ccxx::ParamDecl *ccxx::Parser::parseParamDef(uint16_t pix) {
    auto identTok = lexer.next(TokenKind::identifier);
    auto &identName = std::get<llvm::SmallString<0>>(identTok.V);
    TypeExpr *typeExpr = nullptr;
    Expr *initExpr = nullptr;

    if (lexer.lookaheadDiscard(TokenKind::colon)) {
        typeExpr = parseTypeExpression();
    }

    if (lexer.lookaheadDiscard(TokenKind::equal)) {
        initExpr = parseExpression();
    }

    // parent of param def should be function
    return astContext.emplace<ParamDecl>(pix, typeExpr ? typeExpr->getResolvedType() : astContext.getGenericType(),
                                         identName, astContext.getCurrentContext(), initExpr, typeExpr);
}

// ccxx::NamespaceDef *ccxx::Parser::parseNamespaceDef() {
//     assert(lexer.lookaheadMatch(
//         {TokenKind::identifier, TokenKind::colonequal, TokenKind::tok_namespace, TokenKind::l_brace}));
//     auto identTok = lexer.next();
//     //    auto &identName = std::get<llvm::StringRef>(identTok.V);
//
//     lexer.next(); // eat :
//     lexer.next(); // eat namespace
//     lexer.next(); // eat {
//
//     auto ns = astContext.emplace<NamespaceDef>(astContext.getTopLevel());
//     astContext.pushContext(ns);
//
//     while (!lexer.lookaheadMatch(TokenKind::r_brace)) {
//         auto childDef = parseDefinition();
//         if (childDef) {
//             ns->addChild(childDef);
//         } else {
//             auto bad_token = lexer.next();
//             //            llvm::errs() << fmt::format("{}:{}:{} error: expecting
//             //            a definition\n", "<placeholder for file path>",
//             //            bad_token.start.lineNo(), bad_token.start.columnNo());
//             std::exit(-1);
//         }
//     }
//     lexer.next(); // eat }
//     astContext.popContext();
//     return ns;
//     return {};
// }

ccxx::ValueDecl *ccxx::Parser::parseValueDef() {
    lexer.skip(TokenKind::tok_let);
    auto identifierToken = lexer.next(TokenKind::identifier);
    auto &identifierStr = std::get<llvm::SmallString<0>>(identifierToken.V);

    if (astContext.getCurrentContext()->resolveNarrow(identifierStr) != nullptr) {
        sourceMgr.PrintMessage(identifierToken.loc, llvm::SourceMgr::DiagKind::DK_Error,
                               llvm::formatv("'{0}' is already defined", identifierStr));
        exit(-1);
    }
    TypeExpr *typeExpr = nullptr;
    Expr *initExpr = nullptr;

    if (lexer.lookaheadMatch({TokenKind::colon})) {
        lexer.skip(TokenKind::colon);
        typeExpr = parseTypeExpression();
    }

    if (lexer.lookaheadMatch({TokenKind::equal})) {
        lexer.skip(TokenKind::equal);
        initExpr = parseExpression();
    }

    if (!lexer.lookaheadDiscard(TokenKind::semicolon)) {
        sourceMgr.PrintMessage(lexer.currentLoc(), DiagKind::DK_Error, "Expected 'semicolon' after definition");
        exit(-1);
    }

    auto valueDecl = astContext.emplace<ValueDecl>(typeExpr ? typeExpr->getResolvedType() : initExpr->resultType(),
                                                   identifierStr, astContext.getCurrentContext(), initExpr, typeExpr);

    if (typeExpr) {
        valueDecl->addChild(astContext.emplace<ValueTypeDecl>(valueDecl, typeExpr));
    }

    if (initExpr) {
        valueDecl->addChild(astContext.emplace<ValueInitializerDecl>(valueDecl, initExpr));
    }

    return valueDecl;
}

const ccxx::BuiltinType *ccxx::Parser::parseBuiltinType() {
    TypeModifiers mods;
    llvm::SMLoc lastLoc;

    while (lexer.lookaheadMatch(ccxx::isTypeModifier)) {
        auto mod = lexer.next();
        lastLoc = mod.loc;

        switch (mod.kind) {
        case TokenKind::tok_short:
            if (!mods.Long && !mods.LongLong && !mods.Short) {
                mods.Short = true;
            } else if (mods.LongLong || mods.Long) {
                sourceMgr.PrintMessage(mod.loc, DiagKind::DK_Error,
                                       "'long' and 'short' modifiers are mutually exclusive");
                exit(-1);
            } else {
                sourceMgr.PrintMessage(mod.loc, DiagKind::DK_Error, "Too many 'short' modifiers");
                exit(-1);
            }
            break;
        case TokenKind::tok_long:
            if (!mods.Short && !mods.LongLong) {
                if (mods.Long) {
                    mods.LongLong = true;
                } else {
                    mods.Long = true;
                }
            } else if (mods.Short) {
                sourceMgr.PrintMessage(mod.loc, DiagKind::DK_Error,
                                       "'long' and 'short' modifiers are mutually exclusive");
                exit(-1);
            } else {
                sourceMgr.PrintMessage(mod.loc, DiagKind::DK_Error, "Too many 'long' qualifiers");
                exit(-1);
            }
            break;
        case TokenKind::tok_signed:
            if (mods.Sign == Signedness::Undefined) {
                mods.Sign = Signedness::Signed;
            } else {
                sourceMgr.PrintMessage(mod.loc, DiagKind::DK_Error, "Signedness may only be specified once");
                exit(-1);
            }
        case TokenKind::tok_unsigned:
            if (mods.Sign == Signedness::Undefined) {
                mods.Sign = Signedness::Unsigned;
            } else {
                sourceMgr.PrintMessage(mod.loc, DiagKind::DK_Error, "Signedness may only be specified once");
                exit(-1);
            }
            break;
        default: assert(false);
        }
    }

    FundamentalType ty = FundamentalType::ft_int;
    if (lexer.lookaheadMatch(isFundamentalType)) {
        auto ftTok = lexer.next();
        lastLoc = ftTok.loc;
        ty = getFundamentalTypeFromToken(ftTok.kind);
        assert(ty != FundamentalType::ft_invalid);
    }
    const BuiltinType *type = nullptr;

    switch (ty) {
    case FundamentalType::ft_int:
        if (mods.Long) {
            if (mods.LongLong) {
                if (mods.Sign == Signedness::Unsigned) {
                    type = astContext.getBuiltinType(BuiltinKind::UnsignedLongLong);
                } else {
                    type = astContext.getBuiltinType(BuiltinKind::SignedLongLong);
                }
            } else {
                if (mods.Sign == Signedness::Unsigned) {
                    type = astContext.getBuiltinType(BuiltinKind::UnsignedLongLong);
                } else {
                    type = astContext.getBuiltinType(BuiltinKind::SignedLong);
                }
            }
        } else if (mods.Short) {
            if (mods.Sign == Signedness::Unsigned) {
                type = astContext.getBuiltinType(BuiltinKind::UnsignedShort);
            } else {
                type = astContext.getBuiltinType(BuiltinKind::SignedShort);
            }
        } else {
            if (mods.Sign == Signedness::Unsigned) {
                type = astContext.getBuiltinType(BuiltinKind::UnsignedInt);
            } else {
                type = astContext.getBuiltinType(BuiltinKind::SignedInt);
            }
        }
        break;
    case FundamentalType::ft_char:
        if (mods.Long || mods.LongLong) {
            sourceMgr.PrintMessage(lastLoc, DiagKind::DK_Error, "'char' type does not accept 'long' modifiers");
            exit(-1);
        }
        if (mods.Short) {
            sourceMgr.PrintMessage(lastLoc, DiagKind::DK_Error, "'char' type does not accept 'short' modifiers");
            exit(-1);
        }

        if (mods.Sign == Signedness::Unsigned) {
            type = astContext.getBuiltinType(BuiltinKind::UnsignedChar);
        } else {
            type = astContext.getBuiltinType(BuiltinKind::SignedChar);
        }
        break;
    case FundamentalType::ft_void:
        if (mods.Long || mods.LongLong || mods.Short || mods.Sign != Signedness::Undefined) {
            sourceMgr.PrintMessage(lastLoc, DiagKind::DK_Error, "'void' type does not accept any modifiers");
            exit(-1);
        }
        type = astContext.getBuiltinType(BuiltinKind::Void);
        break;
    case FundamentalType::ft_bool:
        if (mods.Long || mods.LongLong || mods.Short || mods.Sign != Signedness::Undefined) {
            sourceMgr.PrintMessage(lastLoc, DiagKind::DK_Error, "'bool' type does not accept any modifiers");
            exit(-1);
        }
        type = astContext.getBuiltinType(BuiltinKind::Bool);
        break;
    case FundamentalType::ft_float:
        if (mods.Long || mods.LongLong || mods.Short || mods.Sign != Signedness::Undefined) {
            sourceMgr.PrintMessage(lastLoc, DiagKind::DK_Error, "'float' type does not accept any modifiers");
            exit(-1);
        }
        type = astContext.getBuiltinType(BuiltinKind::Float);
        break;
    case FundamentalType::ft_double:
        if (mods.Long || mods.LongLong || mods.Short || mods.Sign != Signedness::Undefined) {
            sourceMgr.PrintMessage(lastLoc, DiagKind::DK_Error, "'double' type does not accept any modifiers");
            exit(-1);
        }
        type = astContext.getBuiltinType(BuiltinKind::Double);
        break;
    default: break;
    }
    return type;
}

ccxx::TypeExpr *ccxx::Parser::parseTypeExpression() {

    struct PointerLevel {
        uint8_t quals = TypeQualBits::tq_noQualsBit;
    };

    llvm::SmallVector<PointerLevel, 2> pointerLevels;
    const Type *leafType = nullptr;
    uint8_t quals = TypeQualBits::tq_noQualsBit;
    while (true) {

        if (lexer.lookaheadMatch(isTypeQualifier)) {
            auto qualTok = lexer.next();
            // only const qualifier exists as of now
            assert(qualTok.kind == TokenKind::tok_const);
            quals |= TypeQualBits::tq_constBit;
            continue;
        }

        if (lexer.lookaheadDiscard(TokenKind::star)) {
            pointerLevels.push_back(PointerLevel{quals});
            quals = TypeQualBits::tq_noQualsBit;
            continue;
        }

        if (lexer.lookaheadMatch(isFundamentalType) || lexer.lookaheadMatch(isTypeModifier)) {
            leafType = parseBuiltinType();
            break;
        }
        // @todo: handle user types
        // @todo: handle functions returning types
        // @todo: report error
    }

    QualType qualifiedResultType = {leafType, quals};
    while (!pointerLevels.empty()) {
        auto pointerLevel = pointerLevels.pop_back_val();
        qualifiedResultType = QualType{astContext.getPointerType(qualifiedResultType), pointerLevel.quals};
    }

    return astContext.emplace<TypeExpr>(astContext, qualifiedResultType);
}
