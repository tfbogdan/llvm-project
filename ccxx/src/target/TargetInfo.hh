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
      protected:
        unsigned charWidth = 0;
        unsigned shortWidth = 0;
        unsigned intWidth = 0;
        unsigned longWidth = 0;
        unsigned lLongWidth = 0;

      public:
        virtual ~TargetInfo() = 0;
        static std::unique_ptr<TargetInfo> CreateTargetInfo();

        unsigned GetIntTypeWidth(IntTypes intType) const;
    };

    class X86TargetInfo : public TargetInfo {
      public:
        X86TargetInfo();
        ~X86TargetInfo() override;
    };
}

