target_include_directories(ccxx PRIVATE ${CMAKE_CURRENT_LIST_DIR} ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(ast)
add_subdirectory(lex)
add_subdirectory(parse)
add_subdirectory(type)
add_subdirectory(codegen)
add_subdirectory(semantics)
add_subdirectory(target)

#target_link_libraries(ad0 PRIVATE ${FLEX_LIBRARIES})

#add_custom_command(
#    ${FLEX_EXECUTABLE} -o${CMAKE_CURRENT_BINARY_DIR}/lexer.cc ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l
#    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/lexer.cc
#    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l
#    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/lexer.cc
#    )

#target_sources(ad0 PRIVATE -o${CMAKE_CURRENT_BINARY_DIR}/lexer.cc)
