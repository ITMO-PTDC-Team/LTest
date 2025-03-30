#pragma once

#include <cstdint>
struct FutexState {
    std::intptr_t addr;
    long value;

    inline bool CanBeBlocked() {
        return *reinterpret_cast<int *>(addr) == value;
    }
};