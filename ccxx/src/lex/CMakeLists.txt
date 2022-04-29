add_llvm_library(ccxx_lex STATIC
        TokenKindsClearMacros.def TokenKinds.def TokenKinds.hh TokenKinds.cc
        Lexer.hh Lexer.cc
        )

target_compile_features(ccxx_lex PUBLIC cxx_std_20)
target_link_libraries(ccxx_lex PUBLIC ${llvm_libs})

