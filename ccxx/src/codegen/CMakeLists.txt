target_sources(ccxx PRIVATE
        IrGenerator.cc
        IrGenerator.hh)

target_precompile_headers(ccxx PRIVATE
        IrGenerator.hh
        )