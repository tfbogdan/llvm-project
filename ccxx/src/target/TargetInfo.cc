#include "TargetInfo.hh"

std::unique_ptr<ccxx::TargetInfo> ccxx::TargetInfo::CreateTargetInfo() {
    return std::make_unique<ccxx::TargetInfo>();
}

unsigned ccxx::TargetInfo::GetIntTypeWidth(ccxx::IntTypes intType) const {
    switch(intType) {
    case IntTypes::SignedChar: [[fallthrough]];
    case IntTypes::UnsignedChar:
        return charWidth;
    case IntTypes::UnsignedShort: [[fallthrough]];
    case IntTypes::SignedShort
        return shortWidth;
    case IntTypes::SignedInt: [[fallthrough]];
    case IntTypes::UnsignedInt:
        return intWidth;
    case IntTypes::SignedLong: [[fallthrough]];
    case IntTypes::UnsignedLong:
        return longWidth;
    case IntTypes::SignedLongLong: [[fallthrough]];
    case IntTypes::UnsignedLongLong:
        return lLongWidth;
    default:
        assert(false);
        break;
    }
    return 0;
}