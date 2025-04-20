#include "include/generators.h"

namespace ltest {

namespace generators {

std::shared_ptr<Token> generated_token{};

// Generates empty arguments.
std::tuple<> GenEmpty(size_t thread_num) { return std::tuple<>(); }

// Generates runtime token.
// Can be called only once per task creation.
std::tuple<std::shared_ptr<Token>> GenToken(size_t thread_num) {
  assert(!generated_token && "forgot to reset generated_token");
  generated_token = std::make_shared<Token>();
  return {generated_token};
}

}  // namespace generators

}  // namespace ltest
