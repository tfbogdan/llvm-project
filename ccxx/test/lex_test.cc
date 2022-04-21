#include <gtest/gtest.h>

#include <lex/Lexer.hh>
#include <string_view>

#include <chrono>

using Lexer = ccxx::Lexer;
using Token = ccxx::Token;
using tok = ccxx::TokenKind;

using namespace std::string_view_literals;

TEST(CXXLex, Empty) {
    Lexer l("");
    auto res = l.next();
    EXPECT_EQ(res.kind, tok::eof);
}

TEST(CXXLex, Number_Binary) {
    Lexer lex{"0B00000011"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);
    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 0B00000011ull);
}

TEST(CXXLex, Number_binary) {
    Lexer lex{"0b00000011"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);

    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 0B00000011ull);
}

TEST(CXXLex, Number_binary_separated) {
    Lexer lex{"0b0000001'11110000"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 0b0000001'11110000);
}

TEST(CXXLex, Number_binary_repeatedseparator) {
    Lexer lex{"0b0000001''11110000"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_TRUE(res.isDirty);
    EXPECT_EQ(std::get<llvm::APInt>(res.V), std::numeric_limits<uint64_t>::max());
}

TEST(CXXLex, Number_binary_overflowing) {
    Lexer lex{"0b1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_TRUE(res.isOverflowing);
    EXPECT_EQ(std::get<llvm::APInt>(res.V), std::numeric_limits<uint64_t>::max());
}

TEST(CXXLex, Number_binary_MaxULL) {
    Lexer lex{"0b1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_EQ(std::get<llvm::APInt>(res.V), std::numeric_limits<unsigned long long>::max());
}


TEST(CXXLex, Number_heX) {
    Lexer lex{"0X0000FF00"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);

    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 0X0000FF00);
}

TEST(CXXLex, Number_hex) {
    Lexer lex{"0x0000FF0000"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);

    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 0x0000FF0000);
}

TEST(CXXLex, Number_hex_separated) {
    Lexer lex{"0x00'fF'0f"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);

    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 0x00'fF'0f);
}

TEST(CXXLex, Number_hex_repeatedseparator) {
    Lexer lex{"0x00''fF'0f"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_TRUE(res.isDirty);
    EXPECT_EQ(std::get<llvm::APInt>(res.V), std::numeric_limits<uint64_t>::max());
}

TEST(CXXLex, Number_hex_overflowing) {
    Lexer lex{"0XFF'00'FF'00'FF'00'FF'ff'00"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_TRUE(res.isOverflowing);
    EXPECT_EQ(std::get<llvm::APInt>(res.V), std::numeric_limits<uint64_t>::max());
}

TEST(CXXLex, Number_hex_MaxULL) {
    Lexer lex{"0xFF'FF'FF'FF'FF'FF'ff'ff"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_EQ(std::get<llvm::APInt>(res.V), std::numeric_limits<uint64_t>::max());
}

TEST(CXXLex, Number_Decimal) {
    Lexer lex{"160990"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);

    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 160990);
}

TEST(CXXLex, Number_Decimal_Separators) {
    Lexer lex{"16'09'90"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_FALSE(res.isDirty);
    EXPECT_FALSE(res.isOverflowing);

    auto apInt = std::get<llvm::APInt>(res.V);
    EXPECT_EQ(apInt.getBitWidth(), 64);
    EXPECT_EQ(apInt, 160990);
}


TEST(CXXLex, Number_Decimal_Suffix) {
    Lexer lex{"16'09'90d"};
    auto res = lex.next();

    EXPECT_EQ(res.kind, tok::integral_constant);
    EXPECT_TRUE(res.hasSuffix);

    res = lex.next();
    EXPECT_EQ(res.kind, tok::identifier);
    EXPECT_EQ(std::get<llvm::StringRef>(res.V), "d");
}


// Not really a sensible test
// TEST(CXXLex, Number_binary_perf) {
//    const auto start_time_lex = std::chrono::steady_clock::now();
//    constexpr auto numcycles = 10000000u;
//    for (size_t ix = 0; ix < numcycles; ++ix) {
//        Lexer lex{"0b00100111"};
//        lex.lexNumber();
//    }
//    const auto end_time_lex = std::chrono::steady_clock::now();

//    char* end;
//    for (size_t ix = 0; ix < numcycles; ++ix) {
//        strtoull("0b00100111", &end, 0);
//    }
//    const auto end_time_std = std::chrono::steady_clock::now();

//    const auto duration_lex = std::chrono::nanoseconds(end_time_lex - start_time_lex);
//    const auto duration_std = std::chrono::nanoseconds(end_time_std - end_time_lex);

//    constexpr auto nanosecs_per_sec = 1000000000.;
//    EXPECT_GT(duration_std.count() / nanosecs_per_sec, duration_lex.count() / nanosecs_per_sec);
//}
