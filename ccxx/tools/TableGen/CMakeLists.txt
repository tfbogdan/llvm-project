
set(LLVM_LINK_COMPONENTS Support)

add_tablegen(ccxx-tablegen CCXX
        CCXXTableGenBackends.cc
        CCXXTableGenBackends.hh

        ASTNodeRecord.hh
        ASTNodeRecord.cc
)

target_compile_features(ccxx-tablegen PUBLIC cxx_std_20)
set_source_files_properties(CCXXTableGenBackends.cc ASTNodeRecord.cc COMPILE_FEATURES cxx_std_20)
