target_sources(ccxx PRIVATE
        SemanticAnalyzer.cc
        SemanticAnalyzer.hh
        )

target_precompile_headers(ccxx PRIVATE
        SemanticAnalyzer.hh)