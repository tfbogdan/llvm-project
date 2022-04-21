target_sources(ccxx PRIVATE
        TargetInfo.hh TargetInfo.cc
        )

target_precompile_headers(ccxx PRIVATE
        TargetInfo.hh
        )

