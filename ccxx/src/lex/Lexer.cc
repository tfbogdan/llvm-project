#include "Lexer.hh"

#include "TokenKinds.hh"
#include <array>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Support/raw_ostream.h>

enum CharFlags : unsigned
{
    Char_HexDigitMask = 0xf, // Keep first 4 bits for the hex digit
    Char_OctalDigitMask = 07 << ccxx::required_bits<Char_HexDigitMask>(),
    Char_HWhitespace = 1U << ccxx::required_bits<Char_OctalDigitMask>(),
    Char_IdentifierBody = 1U << ccxx::required_bits<Char_HWhitespace>(),
    Char_LowerCase = 1U << ccxx::required_bits<Char_IdentifierBody>(),
    Char_UpperCase = 1U << ccxx::required_bits<Char_LowerCase>(),
    Char_BinaryDigit = 1U << ccxx::required_bits<Char_UpperCase>(),
    Char_OctalDigit = 1U << ccxx::required_bits<Char_BinaryDigit>(),
    Char_DecDigit = 1U << ccxx::required_bits<Char_OctalDigit>(),
    Char_HexDigit = 1U << ccxx::required_bits<Char_DecDigit>(),
    Char_VWhitespace = 1U << ccxx::required_bits<Char_HexDigit>(),
    Char_NumberSeparator = 1U << ccxx::required_bits<Char_VWhitespace>(),
    Char_IdentifierStarter = 1U << ccxx::required_bits<Char_NumberSeparator>(),
    // For '.', e and E which when encountered during a decimal number parse,
    // make it become a real number parse Could come up with a better name
    Char_RealNumberStarter = 1U << ccxx::required_bits<Char_IdentifierStarter>()
};

namespace {

constexpr auto char_traits = std::array<unsigned, 128>{
    /* 0   NULL	(Null character)                                     */ 0,
    /* 1   SOH	(Start of Header)                                    */ 0,
    /* 2   STX	(Start of Text)                                      */ 0,
    /* 3   ETX	(End of Text)                                        */ 0,
    /* 4   EOT	(End of Transmission)                                */ 0,
    /* 5   ENQ	(Enquiry)                                            */ 0,
    /* 6   ACK	(Acknowledgement)                                    */ 0,
    /* 7   BEL	(Bell)                                               */ 0,
    /* 8   BS	(Backspace)                                          */ 0,
    /* 9   HT	(Horizontal Tab)                                     */ Char_HWhitespace,
    /* 10  LF	(Line feed)                                          */ Char_VWhitespace,
    /* 11  VT	(Vertical Tab)                                       */ Char_HWhitespace,
    /* 12  FF	(Form feed)                                          */ Char_HWhitespace,
    /* 13  CR	(Carriage return)                                    */ Char_VWhitespace,
    /* 14  SO	(Shift Out)                                          */ 0,
    /* 15  SI	(Shift In)                                           */ 0,
    /* 16  DLE	(Data link escape)                                   */ 0,
    /* 17  DC1	(Device control 1)                                   */ 0,
    /* 18  DC2	(Device control 2)                                   */ 0,
    /* 19  DC3	(Device control 3)                                   */ 0,
    /* 20  DC4	(Device control 4)                                   */ 0,
    /* 21  NAK	(Negative acknowledgement)                           */ 0,
    /* 22  SYN	(Synchronous idle)                                   */ 0,
    /* 23  ETB	(End of transmission block)                          */ 0,
    /* 24  CAN	(Cancel)                                             */ 0,
    /* 25  EM	(End of medium)                                      */ 0,
    /* 26  SUB	(Substitute)                                         */ 0,
    /* 27  ESC	(Escape)                                             */ 0,
    /* 28  FS	(File separator)                                     */ 0,
    /* 29  GS	(Group separator)                                    */ 0,
    /* 30  RS	(Record separator)                                   */ 0,
    /* 31  US	(Unit separator)                                     */ 0,
    /* 32  ' '  (space)                                              */ Char_HWhitespace,
    /* 33  !	(exclamation mark)                                   */ 0,
    /* 34  "	(Quotation mark)                                     */ 0,
    /* 35  #	(Number sign)                                        */ 0,
    /* 36  $	(Dollar sign)                                        */ 0,
    /* 37  %	(Percent sign)                                       */ 0,
    /* 38  &	(Ampersand)                                          */ 0,
    /* 39  '	(Apostrophe)                                         */ Char_NumberSeparator,
    /* 40  (	(round brackets or parentheses)                      */ 0,
    /* 41  )	(round brackets or parentheses)                      */ 0,
    /* 42  *	(Asterisk)                                           */ 0,
    /* 43  +	(Plus sign)                                          */ 0,
    /* 44  ,	(Comma)                                              */ 0,
    /* 45  -	(Hyphen)                                             */ 0,
    /* 46  .	(Full stop , dot)                                    */ Char_RealNumberStarter,
    /* 47  /	(Slash)                                              */ 0,
    /* 48  0	(number zero)                                        */ Char_IdentifierBody | Char_BinaryDigit |
        Char_OctalDigit | Char_DecDigit | Char_HexDigit,
    /* 49  1	(number one)                                         */ Char_IdentifierBody | Char_BinaryDigit |
        Char_OctalDigit | Char_DecDigit | Char_HexDigit | 0x1u | 01 << 4u,
    /* 50  2	(number two)                                         */ Char_IdentifierBody | Char_OctalDigit |
        Char_DecDigit | Char_HexDigit | 0x2u | 02u << 4u,
    /* 51  3	(number three)                                       */ Char_IdentifierBody | Char_OctalDigit |
        Char_DecDigit | Char_HexDigit | 0x3u | 03u << 4u,
    /* 52  4	(number four)                                        */ Char_IdentifierBody | Char_OctalDigit |
        Char_DecDigit | Char_HexDigit | 0x4u | 04u << 4u,
    /* 53  5	(number five)                                        */ Char_IdentifierBody | Char_OctalDigit |
        Char_DecDigit | Char_HexDigit | 0x5u | 05u << 4u,
    /* 54  6	(number six)                                         */ Char_IdentifierBody | Char_OctalDigit |
        Char_DecDigit | Char_HexDigit | 0x6u | 06u << 4u,
    /* 55  7	(number seven)                                       */ Char_IdentifierBody | Char_OctalDigit |
        Char_DecDigit | Char_HexDigit | 0x7u | 07u << 4u,
    /* 56  8	(number eight)                                       */ Char_IdentifierBody | Char_DecDigit |
        Char_HexDigit | 0x8u,
    /* 57  9	(number nine)                                        */ Char_IdentifierBody | Char_DecDigit |
        Char_HexDigit | 0x9u,
    /* 58  :	(Colon)                                              */ 0,
    /* 59  ;	(Semicolon)                                          */ 0,
    /* 60  <	(Less-than sign )                                    */ 0,
    /* 61  =	(Equals sign)                                        */ 0,
    /* 62  >	(Greater-than sign ; Inequality)                     */ 0,
    /* 63  ?	(Question mark)                                      */ 0,
    /* 64  @	(At sign)                                            */ 0,
    /* 65  A	(Capital A )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase | Char_HexDigit | 0xA,
    /* 66  B	(Capital B )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase | Char_HexDigit | 0xB,
    /* 67  C	(Capital C )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase | Char_HexDigit | 0xC,
    /* 68  D	(Capital D )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase | Char_HexDigit | 0xD,
    /* 69  E	(Capital E )                                         */ Char_RealNumberStarter |
        Char_IdentifierStarter | Char_IdentifierBody | Char_UpperCase | Char_HexDigit | 0xE,
    /* 70  F	(Capital F )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase | Char_HexDigit | 0xF,
    /* 71  G	(Capital G )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 72  H	(Capital H )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 73  I	(Capital I )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 74  J	(Capital J )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 75  K	(Capital K )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 76  L	(Capital L )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 77  M	(Capital M )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 78  N	(Capital N )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 79  O	(Capital O )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 80  P	(Capital P )                                         */ Char_RealNumberStarter |
        Char_IdentifierStarter | Char_IdentifierBody | Char_UpperCase,
    /* 81  Q	(Capital Q )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 82  R	(Capital R )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 83  S	(Capital S )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 84  T	(Capital T )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 85  U	(Capital U )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 86  V	(Capital V )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 87  W	(Capital W )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 88  X	(Capital X )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 89  Y	(Capital Y )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 90  Z	(Capital Z )                                         */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_UpperCase,
    /* 91  [	(square brackets or box brackets)                    */ 0,
    /* 92  \	(Backslash)                                          */ 0,
    /* 93  ]	(square brackets or box brackets)                    */ 0,
    /* 94  ^	(Caret or circumflex accent)                         */ 0,
    /* 95  _	(underscore , understrike , underbar or low line)    */ Char_IdentifierStarter | Char_IdentifierBody,
    /* 96  `	(Grave accent)                                       */ 0,
    /* 97  a	(Lowercase  a )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase | Char_HexDigit | 0xA,
    /* 98  b	(Lowercase  b )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase | Char_HexDigit | 0xB,
    /* 99  c	(Lowercase  c )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase | Char_HexDigit | 0xC,
    /* 100 d	(Lowercase  d )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase | Char_HexDigit | 0xD,
    /* 101 e	(Lowercase  e )                                      */ Char_RealNumberStarter |
        Char_IdentifierStarter | Char_IdentifierBody | Char_LowerCase | Char_HexDigit | 0xE,
    /* 102 f	(Lowercase  f )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase | Char_HexDigit | 0xF,
    /* 103 g	(Lowercase  g )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 104 h	(Lowercase  h )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 105 i	(Lowercase  i )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 106 j	(Lowercase  j )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 107 k	(Lowercase  k )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 108 l	(Lowercase  l )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 109 m	(Lowercase  m )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 110 n	(Lowercase  n )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 111 o	(Lowercase  o )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 112 p	(Lowercase  p )                                      */ Char_RealNumberStarter |
        Char_IdentifierStarter | Char_IdentifierBody | Char_LowerCase,
    /* 113 q	(Lowercase  q )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 114 r	(Lowercase  r )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 115 s	(Lowercase  s )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 116 t	(Lowercase  t )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 117 u	(Lowercase  u )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 118 v	(Lowercase  v )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 119 w	(Lowercase  w )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 120 x	(Lowercase  x )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 121 y	(Lowercase  y )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 122 z	(Lowercase  z )                                      */ Char_IdentifierStarter | Char_IdentifierBody |
        Char_LowerCase,
    /* 123 {	(curly brackets or braces)                           */ 0,
    /* 124 |	(vertical-bar, vbar, vertical line or vertical slash)*/ 0,
    /* 125 }	(curly brackets or braces)                           */ 0,
    /* 126 ~	(Tilde ; swung dash)                                 */ 0,
    /* 127 DEL	(Delete)                                             */ 0};

} // namespace

bool isWhitespace(char C) {
    return char_traits[C] & Char_HWhitespace;
}

ccxx::Token ccxx::Lexer::next() {
    auto result = nextImpl();
    switch (result.kind) {
    case l_paren: ++paren_depth; break;
    case r_paren: --paren_depth; break;
    case l_square: ++square_depth; break;
    case r_square: --square_depth; break;
    case l_brace: ++brace_depth; break;
    case r_brace: --brace_depth; break;
    default: break;
    }
    return result;
}

ccxx::Token ccxx::Lexer::next(TokenKind ExpectedKind) {
    auto res = next();
    assert(res.kind == ExpectedKind);
    return res;
}

void ccxx::Lexer::skip(TokenKind k) {
    auto tok = next();
    assert(k == tok.kind);
}

std::deque<ccxx::Token> ccxx::Lexer::lookahead(const size_t num) {

    if (lookaheadQueue.size() == num) {
        return lookaheadQueue;
    }

    while (lookaheadQueue.size() < num) {
        lookaheadQueue.emplace_front(lex());
        if (lookaheadQueue.back() == TokenKind::eof) {
            break;
        }
    }
    auto beg = lookaheadQueue.end() - std::max(num, lookaheadQueue.size());
    return std::deque<Token>{beg, lookaheadQueue.end()};
}

std::deque<ccxx::Token> ccxx::Lexer::eatUntilNextCurlyBrace() {
    auto tok = next();
    std::deque<ccxx::Token> result;
    while (tok != TokenKind::r_brace && brace_depth != 0) {
        result.emplace_front(std::move(tok));
        tok = next();
    }
    return result;
}

bool ccxx::Lexer::lookaheadMatch(llvm::ArrayRef<ccxx::TokenKind> expected) {
    while (lookaheadQueue.size() < expected.size()) {
        lookaheadQueue.emplace_front(lex());
        if (lookaheadQueue.back() == TokenKind::eof) {
            break;
        }
    }

    if (lookaheadQueue.size() < expected.size()) {
        return false;
    }

    auto beg = lookaheadQueue.rbegin();
    auto end = lookaheadQueue.rbegin() + expected.size();
    const auto *guard = expected.begin();

    for (const auto &t : llvm::iterator_range(beg, end)) {
        if (t != *guard) {
            return false;
        }
        ++guard;
    }
    return true;
}

#define _a case 'a':
#define _b case 'b':
#define _c case 'c':
#define _d case 'd':
#define _e case 'e':
#define _f case 'f':
#define _g case 'g':
#define _h case 'h':
#define _i case 'i':
#define _j case 'j':
#define _k case 'k':
#define _l case 'l':
#define _m case 'm':
#define _n case 'n':
#define _o case 'o':
#define _p case 'p':
#define _q case 'q':
#define _r case 'r':
#define _s case 's':
#define _t case 't':
#define _u case 'u':
#define _v case 'v':
#define _w case 'w':
#define _x case 'x':
#define _y case 'y':
#define _z case 'z':

#define _A case 'A':
#define _B case 'B':
#define _C case 'C':
#define _D case 'D':
#define _E case 'E':
#define _F case 'F':
#define _G case 'G':
#define _H case 'H':
#define _I case 'I':
#define _J case 'J':
#define _K case 'K':
#define _L case 'L':
#define _M case 'M':
#define _N case 'N':
#define _O case 'O':
#define _P case 'P':
#define _Q case 'Q':
#define _R case 'R':
#define _S case 'S':
#define _T case 'T':
#define _U case 'U':
#define _V case 'V':
#define _W case 'W':
#define _X case 'X':
#define _Y case 'Y':
#define _Z case 'Z':
#define __ case '_':

#define _0 case '0':
#define _1 case '1':
#define _2 case '2':
#define _3 case '3':
#define _4 case '4':
#define _5 case '5':
#define _6 case '6':
#define _7 case '7':
#define _8 case '8':
#define _9 case '9':

#define _Dot case '.':
#define _NumberSeparator case '\'':

ccxx::Token ccxx::Lexer::lex() {
    skipWhitespace();

    //    const char* tokenStart = buffer.pos();
    Token tok;
    tok.loc = llvm::SMLoc::getFromPointer(buffer.pos());
    while (tok.kind == not_really_a_token) {

        auto Char = buffer.peekChar();
        switch (Char) {
        case 0: tok.kind = ccxx::eof; break;
        case '\r': {
            auto lf = buffer.advanceAndPeekChar();
            assert(lf == '\n'); // TDO: Diag
        }
            [[fallthrough]];
        case '\n':
            ++line;
            buffer.advance();
            lineStart = buffer.pos();
            break;
        case ' ':
        case '\t':
        case '\f':
        case '\v':
            skipWhitespace();
            break;
            _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 lexNumber(tok);
            break; // numeric literal

            _A _B _C _D _E _F _G _H _I _J _K _L _M _N _O _P _Q _R _S _T _U _V _W _X _Y _Z _a _b _c _d _e _f _g _h _i _j
                _k _l _m _n _o _p _q _r _s _t _u _v _w _x _y _z __ lexIdentifier(tok); // An identifier
            break;

        case '\'': {
            // @todo: there are a lot of error and corner cases that are untreated here;
            buffer.advance();
            tok.kind = TokenKind::char_literal;
            tok.V = static_cast<int>(buffer.peekCharAndAdvance());
            buffer.advance();
        } break;
        case '"': lexString(tok); break;
        case ';':
            buffer.advance();
            tok.kind = semicolon;
            break;

        case ':':
            Char = buffer.advanceAndPeekChar();
            if (Char == '=') {
                tok.kind = TokenKind::colonequal;
                buffer.advance();
            } else if (Char == ':') {
                tok.kind = TokenKind::coloncolon;
                buffer.advance();
            } else {
                tok.kind = TokenKind::colon;
            }
            break;
        case '=':
            Char = buffer.advanceAndPeekChar();
            if (Char == '=') {
                tok.kind = TokenKind::equalequal;
                buffer.advance();
            } else {
                tok.kind = TokenKind::equal;
            }
            break;
        case '?':
            buffer.advance();
            tok.kind = question;
            break;
        case '[':
            buffer.advance();
            tok.kind = l_square;
            break;
        case ']':
            buffer.advance();
            tok.kind = r_square;
            break;
        case '(':
            buffer.advance();
            tok.kind = l_paren;
            break;
        case ')':
            buffer.advance();
            tok.kind = r_paren;
            break;
        case '{':
            buffer.advance();
            tok.kind = l_brace;
            break;
        case '}':
            buffer.advance();
            tok.kind = r_brace;
            break;
        case '+':
            // TDO: handle variations on this, such as ++ += and others
            buffer.advance();
            tok.kind = plus;
            break;
        case '-': {
            Char = buffer.advanceAndPeekChar();
            switch (Char) {
            case '>':
                buffer.advance();
                tok.kind = arrow;
                // TDO: keep looking for ->*
                break;
                // TDO: keep adding cases for others, such as -= -- etc
            default: tok.kind = minus; break;
            }

        } break;
        case '*':
            buffer.advance();
            tok.kind = star;
            break;

        case ',':
            buffer.advance();
            tok.kind = comma;
            break;
        case '.': {
            auto nextChar = buffer.toString()[1];
            if (char_traits[nextChar] & Char_DecDigit) {
                lexNumber(tok);
            } else if (buffer.toString().startswith("...")) {
                buffer.advance(3);
                tok.kind = ellipsis;
            } else {
                buffer.advance();
                tok.kind = ellipsis;
            }
        } break;
        case '/': {
            Char = buffer.advanceAndPeekChar();
            switch (Char) {
            /// Right now, comment tokens are discarded but they should be
            /// stored in some way, for refactoring tools and such
            case '/':
                seek("\n"); /*tok.kind = linecomment;*/
                break;
            case '*':
                seek("*/"); /*tok.kind = multilinecomment;*/
                break;
            case '=': tok.kind = slashequal; break;
            default: tok.kind = slash; break;
            }
        } break;
        }
    }
    tok.paren_depth = paren_depth;
    tok.square_depth = square_depth;
    tok.brace_depth = brace_depth;
    // Value is assumed to have been already placed inside the token by one of
    // the specialized functions (lexNumber etc)
    return tok;
}

ccxx::Token ccxx::Lexer::nextImpl() {
    if (!lookaheadQueue.empty()) {
        auto current = lookaheadQueue.back();
        lookaheadQueue.pop_back();
        return current;
    }

    return lex();
}

void ccxx::Lexer::skipWhitespace() {
    while (isWhitespace(buffer.peekChar())) {
        buffer.advance();
    }
}

void ccxx::Lexer::seek(llvm::StringRef seq) {
    assert(!seq.empty());
    auto remainder = buffer.toString();

    while (!remainder.empty()) {
        if (remainder.startswith(seq)) {
            break;
        }

        if (remainder.front() == '\n') {
            ++line;
            lineStart = buffer.pos();
        }

        buffer.advance();
        remainder = buffer.toString();
    }

    buffer.advance(seq.size());
}

void ccxx::Lexer::lexIdentifier(Token &tok) {
    const char *const identStart = buffer.pos();
    auto C = buffer.peekChar();
    while ((C != 0) and char_traits[C] & Char_IdentifierBody) {
        C = buffer.advanceAndPeekChar();
    }
    const char *identEnd = buffer.pos();

    static const llvm::StringMap<TokenKind> keywords = {
#define KEYWORD(K) {#K, TokenKind::tok_##K},
#include "TokenKinds.def"
#include "TokenKindsClearMacros.def"
    };
    auto identText = llvm::StringRef(identStart, identEnd - identStart);

    if (auto tK = keywords.lookup(identText); tK != not_really_a_token) {
        tok.kind = tK;
    } else {
        tok.kind = identifier;
        tok.V = llvm::SmallString<0>(identText);
    }
}

void ccxx::Lexer::lexNumber(Token &tok) {

    constexpr std::array<unsigned long long, 10> dec_overflow_thresholds = {
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 0,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 1,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 2,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 3,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 4,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 5,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 6,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 7,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 8,
        std::numeric_limits<unsigned long long>::max() / 10 + std::numeric_limits<unsigned long long>::max() % 10 - 9,
    };

    llvm::SmallString<256> real_number_buff;
    llvm::raw_svector_ostream real_number_stream(real_number_buff);

    constexpr auto octal_msb_mask = 0b1110'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    constexpr auto binar_msb_mask = 0b1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    constexpr auto hexad_msb_mask = 0b1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    char C = buffer.peekChar();
    unsigned traits = char_traits[C];
    uint64_t integer_constant = 0;

    switch (C) {
    case '0': {
        C = buffer.advanceAndPeekChar();
        traits = char_traits[C];

        switch (C) {
        case 'b':
        case 'B': {
            C = buffer.advanceAndPeekChar();
            char oldChar = '\'';

            bool error = false;
            bool integer_overflowing = false;

            traits = char_traits[C];
            while (traits & (Char_BinaryDigit | Char_NumberSeparator | Char_DecDigit)) {
                if (traits & Char_BinaryDigit) {
                    unsigned Digit = C - '0';
                    integer_overflowing = integer_overflowing or integer_constant & binar_msb_mask;
                    integer_constant <<= 1u;
                    integer_constant |= Digit;
                } else if (traits & Char_NumberSeparator) {
                    error = error || char_traits[oldChar] & Char_NumberSeparator;
                } else {
                    error = true;
                }
                oldChar = C;
                C = buffer.advanceAndPeekChar();
                traits = char_traits[C];
            }

            if (traits & Char_IdentifierStarter) {
                tok.hasSuffix = true;
            }

            tok.kind = integral_constant;
            tok.isOverflowing = integer_overflowing;
            tok.isDirty = error;
            tok.V = llvm::APInt(
                64, integer_overflowing || error ? std::numeric_limits<uint64_t>::max() : integer_constant, false);
        } break;
        case 'x':
        case 'X': {
            C = buffer.advanceAndPeekChar();
            char oldChar = '\'';

            bool error = false;
            bool integer_overflowing = false;
            uint64_t integer_constant = 0;

            traits = char_traits[C];
            while (traits & (Char_HexDigit | Char_NumberSeparator)) {
                if (traits & Char_HexDigit) {
                    unsigned Digit = traits & Char_HexDigitMask;
                    integer_overflowing = integer_overflowing or ((integer_constant & hexad_msb_mask) != 0u);
                    integer_constant <<= 4u;
                    integer_constant |= Digit;
                } else if (traits & Char_NumberSeparator) {
                    error = error || char_traits[oldChar] & Char_NumberSeparator;
                }
                oldChar = C;
                C = buffer.advanceAndPeekChar();
                traits = char_traits[C];
            }

            if (traits & Char_IdentifierStarter) {
                error = true; // Hex numbers may not have a suffix
            }

            tok.kind = integral_constant;
            tok.isOverflowing = integer_overflowing;
            tok.isDirty = error;
            tok.V = llvm::APInt(
                64, integer_overflowing || error ? std::numeric_limits<uint64_t>::max() : integer_constant, false);
        } break;
        case 'o':
        case 'O': {
            C = buffer.advanceAndPeekChar();
            char oldChar = '\'';

            bool error = false;
            bool integer_overflowing = false;
            uint64_t integer_constant = 0;

            traits = char_traits[C];
            while (traits & (Char_OctalDigit | Char_NumberSeparator | Char_DecDigit)) {
                if (traits & Char_OctalDigit) {
                    unsigned Digit = (traits & Char_OctalDigitMask) >> 4u;
                    integer_overflowing = integer_overflowing or ((integer_constant & octal_msb_mask) != 0u);
                    integer_constant <<= 3u;
                    integer_constant |= Digit;
                } else if (traits & Char_NumberSeparator) {
                    error = error || char_traits[oldChar] & Char_NumberSeparator;
                } else {
                    error = true;
                }
                oldChar = C;
                C = buffer.advanceAndPeekChar();
                traits = char_traits[C];
            }

            if (traits & Char_IdentifierStarter) {
                tok.hasSuffix = true;
            }

            tok.kind = integral_constant;
            tok.isOverflowing = integer_overflowing;
            tok.isDirty = error;
            tok.V = llvm::APInt(
                64, integer_overflowing || error ? std::numeric_limits<uint64_t>::max() : integer_constant, false);
        } break;
        case '.':
            // Obviously a real number
            break;
        default:
            if (traits & Char_DecDigit) {
                // Obviously malformed. Now eat up the malformation
                // TDO: This could also evolve to become a malformed real number
                while (traits & (Char_DecDigit | Char_NumberSeparator)) {
                    C = buffer.advanceAndPeekChar();
                    traits = char_traits[C];
                }
                if (traits & Char_IdentifierStarter) {
                    tok.hasSuffix = true;
                }
            } else {
                if (traits & Char_IdentifierStarter) {
                    tok.hasSuffix = true;
                }
                tok.isDirty = true;
                tok.kind = integral_constant;
            }
        }
    } break;
    case '.':
        // TDO: Could be a period, an ellipsis, a real number, or some invalid
        // token
        break;
    default:
        if (traits & (Char_DecDigit | Char_RealNumberStarter)) {
            bool error = false;
            bool integer_overflowing = false;
            uint64_t integer_constant = 0;

            C = buffer.peekChar();
            char oldChar = '\'';
            real_number_stream << C;

            while (traits & (Char_DecDigit | Char_NumberSeparator)) {
                if (traits & Char_DecDigit) {
                    const unsigned Digit = C - '0';
                    const auto overflow_threshold = dec_overflow_thresholds[Digit];
                    integer_overflowing = integer_overflowing || integer_constant > overflow_threshold;
                    integer_constant *= 10u;
                    integer_constant += Digit;
                } else {
                    error = error || char_traits[oldChar] & Char_NumberSeparator;
                }
                oldChar = C;
                C = buffer.advanceAndPeekChar();
                real_number_stream << C;

                traits = char_traits[C];
            }

            if (traits & Char_RealNumberStarter) {
                // TDO: This turns into a real number
                // Need to figure out the end of this real number
                // If it's a dot, then decimals may follow, then an exponent
                // (optional sign followed by hex) If it's an exponent, then a
                // sign may follow, then all decimals
            } else {

                if (traits & Char_IdentifierStarter) {
                    tok.hasSuffix = true;
                }

                tok.kind = integral_constant;
                tok.isOverflowing = integer_overflowing;
                tok.isDirty = error;
                tok.V = llvm::APInt(
                    64, integer_overflowing || error ? std::numeric_limits<uint64_t>::max() : integer_constant, false);
            }
        }
        break;
    }
}

void ccxx::Lexer::lexString(Token &tok) {
    auto C = buffer.peekChar();
    llvm::SmallString<1024> buff;
    llvm::raw_svector_ostream buff_stream(buff);

    assert(C == '"');
    C = buffer.advanceAndPeekChar();
    while (C != '"') {
        if (C == '\\') {
            C = buffer.peekCharAndAdvance();
            switch (C) {
            case 'r': buff_stream << '\r'; break;
            case 'n': buff_stream << '\n'; break;
            case 't': buff_stream << '\t'; break;
            case 'v': buff_stream << '\v'; break;
            case 'f': buff_stream << '\f'; break;
            case '\\': buff_stream << '\\'; break;
            default:
                // TDO: either an error, or hex codes.
                break;
            }
        } else if (C == '\n') {
            // TDO: error.
            break;
        } else {
            buff_stream << C;
        }
        C = buffer.advanceAndPeekChar();
    }
    if (C == '"') {
        buffer.advance(); // Eat the remaining quote
    } else {
        tok.isDirty = true;
    }
    tok.kind = string_literal;
    tok.V = llvm::SmallString<0>(buff);
}

bool ccxx::Lexer::lookaheadMatch(ccxx::TokenKind expected) {
    return lookaheadMatch(std::array{expected});
}

bool ccxx::Lexer::lookaheadDiscard(TokenKind expected) {
    if (lookaheadMatch(expected)) {
        next();
        return true;
    }
    return false;
}
