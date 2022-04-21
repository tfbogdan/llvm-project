set(LLVM_TARGET_DEFINITIONS Operators.td)
tablegen(CCXX Operators.inc -print-records)

set(LLVM_TARGET_DEFINITIONS DeclNodes.td)
tablegen(CCXX DeclNodes.inc -gen-decl-nodes)

set(LLVM_TARGET_DEFINITIONS StmtNodes.td)
tablegen(CCXX StmtNodes.inc -gen-stmt-nodes)

set(LLVM_TARGET_DEFINITIONS TypeNodes.td)
tablegen(CCXX TypeNodes.inc -print-records)

# tablegen(CLANG)

add_public_tablegen_target(CCXXParseOperatorsTableGen)
set_target_properties( CCXXParseOperatorsTableGen PROPERTIES FOLDER "CCXX tablegenning")
set_property(GLOBAL APPEND PROPERTY CCXX_TABLEGEN_TARGETS CCXXParseOperatorsTableGen)


