target_sources(ccxx PRIVATE
        Type.cc Type.hh
        BuiltinTypes.def
        BuiltinType.hh
        BuiltinType.cc
)

target_precompile_headers(ccxx PRIVATE
        Type.hh
        BuiltinType.hh)