#include "include/generators.h"

namespace ltest {

namespace generators {

// Generates empty arguments.
std::tuple<> genEmpty(size_t thread_num) { return std::tuple<>(); }

}  // namespace generators

}  // namespace ltest
