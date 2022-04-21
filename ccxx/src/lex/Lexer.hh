#pragma once

#include <llvm/Support/Allocator.h>
#include <llvm/Support/MemoryBuffer.h>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Support/SMLoc.h>

#include <lex/TokenKinds.hh>

#include <concepts>
#include <deque>
#include <variant>

namespace ccxx {

template <unsigned Value> constexpr unsigned required_bits() {
    auto v = Value;
    auto shifts = 0u;

    while (v) {
        v >>= 1;
        ++shifts;
    }
    return shifts;
}

struct NoValue {};

///
struct Token {
    Token(TokenKind k, llvm::SMLoc l)
        : kind(k)
        , loc(l) {
    }
    Token() = default;

    inline void reset() {
        kind = not_really_a_token;
        loc = {};
        hasSuffix = false;
        V = {};
    }

    bool operator==(TokenKind tK) const {
        return kind == tK;
    }
    bool operator!=(TokenKind tK) const {
        return kind != tK;
    }

    template <typename T> const T &value() const {
        assert(std::holds_alternative<T>(V));
        return std::get<T>(V);
    }

    //        static_assert (bitsRequired<TokenKind::Token_Count>() < 8, "");
    ccxx::TokenKind kind = not_really_a_token; // : 8;    // tdo: statically assert that all tokens
                                               // can fit in these 9 bits
    /// Used for constant values. WIll be set if the constant has a suffix. EG:
    /// 12u
    unsigned hasSuffix : 1 = false;
    /// Dirty tokens are malformed in some way or another and they are at best a
    /// guess of the lexer but shouldn't be trusted They should lead to build
    /// errors but the build is going to continue as much as possible without
    /// them
    unsigned isDirty : 1 = false;
    /// Used for numeric constants. It is set if the numbers are overflowing
    unsigned isOverflowing : 1 = false;

    signed paren_depth  : 8 = 0;
    signed square_depth : 8 = 0;
    signed brace_depth  : 8 = 0;

    llvm::SMLoc loc;

    using TokenValue = std::variant<NoValue, llvm::APInt, llvm::APFloat, llvm::StringRef, llvm::SmallString<0>, int>;
    TokenValue V;
};

class Lexer {
  public:
    Lexer() = default;
    Lexer(llvm::StringRef s)
        : buffer(s.data(), s.end())
        , lineStart(s.data()) {
    }

    Token next();
    Token next(TokenKind ExpectedKind);
    void skip(TokenKind k);

    std::deque<Token> lookahead(size_t num);
    bool lookaheadMatch(llvm::ArrayRef<TokenKind> expected);
    bool lookaheadMatch(TokenKind expected);
    /// Looks at the next token. If it's of the expected kind, it is discarded and true is returned.
    /// If it isn't of the expected kind, false is returned and the token stays put
    bool lookaheadDiscard(TokenKind expected);

    bool lookaheadMatch(std::predicate<TokenKind> auto &pred) {
        auto tokensAhead = lookahead(1);
        return pred(tokensAhead[0].kind);
    }

    llvm::SMLoc currentLoc() const {
        return llvm::SMLoc::getFromPointer(buffer.pos());
    }

    std::deque<Token> eatUntilNextCurlyBrace();

    int getParenDepth() const {
        return paren_depth;
    }

    int getSquareBraceDepth() const {
        return square_depth;
    }

    int getCurlyBraceDepth() const {
        return brace_depth;
    }

    void setBuffer(const char *S, const char *E) {
        buffer = bufferWrapper(S, E);
        lineStart = S;
        lookaheadQueue.clear();
        line = 0;
        paren_depth = 0;
        square_depth = 0;
        brace_depth = 0;
    }

    /// lexes one of:
    /// Binary constant
    /// Octal constant
    /// Decimal constant
    /// Hexadecimal constant
    /// Real constant
    /// period token
    /// ellipsis token
    /// Assumes S is null terminated
    void lexNumber(Token &tok);

    /// lexes an identifier and returns the position where the identifier ends.
    /// Assumes S is null terminated
    void lexIdentifier(Token &tok);

  private:
    Token lex();
    Token nextImpl();

    class bufferWrapper {
        //            const char* bufferStart = nullptr;
        const char *bufferEnd = nullptr;
        const char *bufferPos = nullptr;

      public:
        bufferWrapper(const char *S, const char *E)
            : // bufferStart(S),
            bufferEnd(E)
            , bufferPos(S) {
        }

        bufferWrapper() = default;
        bufferWrapper &operator=(bufferWrapper &&) = default;

        /// Returns the next character in the buffer but does not advance the
        /// pointer Repeated calls to this will yield the same value
        LLVM_ATTRIBUTE_ALWAYS_INLINE char peekChar() {
            return bufferPos < bufferEnd ? *bufferPos : 0;
        }

        LLVM_ATTRIBUTE_ALWAYS_INLINE char peekCharAndAdvance() {
            return bufferPos < bufferEnd ? *bufferPos++ : 0;
        }

        LLVM_ATTRIBUTE_ALWAYS_INLINE void advance(long num = 1) {
            bufferPos += std::min(num, static_cast<long>(bufferEnd - bufferPos));
        }

        /// Advances the buffer pointer and returns the next char.
        LLVM_ATTRIBUTE_ALWAYS_INLINE char advanceAndPeekChar() {
            advance();
            return bufferPos < bufferEnd ? *bufferPos : 0;
        }

        LLVM_ATTRIBUTE_ALWAYS_INLINE const char *pos() const {
            return bufferPos;
        }

        /// Returns a string of what's left in the buffer
        LLVM_ATTRIBUTE_ALWAYS_INLINE llvm::StringRef toString() const {
            return llvm::StringRef(bufferPos, bufferEnd - bufferPos);
        }

    } buffer;

    void skipWhitespace();
    void seek(llvm::StringRef seq);

    void lexString(Token &tok);

    const char *lineStart = nullptr;

    // Arbitrary limitation: Won't ever be able to nest more than 255 levels of
    // enclosing tokens
    signed paren_depth  : 8 = 0;
    signed square_depth : 8 = 0;
    signed brace_depth  : 8 = 0;

    unsigned line = 0;

    std::deque<Token> lookaheadQueue;
    llvm::SpecificBumpPtrAllocator<Token> tokenAlloc;
};

} // namespace ccxx
