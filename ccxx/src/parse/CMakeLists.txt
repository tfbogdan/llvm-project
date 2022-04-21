target_sources(ccxx PRIVATE
        OperatorInfo.hh OperatorInfo.cc
        Parser.cc Parser.hh
        OperatorInfo.def
        )

target_precompile_headers(ccxx PRIVATE
        Parser.hh
        OperatorInfo.hh)

