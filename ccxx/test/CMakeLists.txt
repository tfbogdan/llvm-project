target_sources(ccxx PRIVATE source.ccxx)

enable_testing()

find_package(GTest CONFIG REQUIRED)
add_executable(lex_test lex_test.cc)
target_link_libraries(lex_test GTest::gtest GTest::gtest_main ccxx_lex)

add_test(lex_test lex_test)
