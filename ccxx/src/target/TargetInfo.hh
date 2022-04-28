#pragma once

#include <cstdint>
#include <memory>

namespace ccxx {

    enum class IntTypes : int8_t {
        NoInt = -1,
        SignedChar,
        UnsignedChar,
        SignedShort,
        UnsignedShort,
        SignedInt,
        UnsignedInt,
        SignedLong,
        UnsignedLong,
        SignedLongLong,
        UnsignedLongLong,
        IntTypeCount = UnsignedLongLong
    };

    enum class FloatTypes : int8_t {
        NoFloat = -1,
        Float,
        Double,
        LongDouble,
        Float128,
        Ibm128
    };

    class TargetInfo {
        unsigned charWidth;
        unsigned shortWidth;
        unsigned intWidth;
        unsigned longWidth;
        unsigned lLongWidth;

      public:
        static std::unique_ptr<TargetInfo> CreateTargetInfo();

        unsigned GetIntTypeWidth(IntTypes intType) const;
    };

}

