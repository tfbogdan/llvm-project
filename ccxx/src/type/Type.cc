#include "Type.hh"

bool ccxx::QualType::operator==(ccxx::QualType rhs) {
    return storage == rhs.storage;
}