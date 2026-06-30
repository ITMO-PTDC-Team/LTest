#include "../../specs/libcoro/ring_buffer.h"

#include "../../../runtime/include/verifying.h"

#include <cstdlib>
#include <coro/expected.hpp>
#include <coro/ring_buffer.hpp>

static auto genInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

using ConsumeValue = tl::expected<int, coro::ring_buffer_result::consume>;

struct LibcoroRingBufferTarget : coro::ring_buffer<int, 1> {
  non_atomic auto produce(int value) {
    return coro::ring_buffer<int, 1>::produce(value);
  }

  non_atomic auto consume() {
    return coro::ring_buffer<int, 1>::consume();
  }

  as_atomic std::size_t size() {
    return coro::ring_buffer<int, 1>::size();
  }

  as_atomic bool empty() {
    return coro::ring_buffer<int, 1>::empty();
  }

  as_atomic bool full() {
    return coro::ring_buffer<int, 1>::full();
  }

  as_atomic spec::LibcoroRingBufferSnapshot snapshot() {
    return {
        .size = this->size(),
        .empty = this->empty(),
        .full = this->full(),
    };
  }
};

target_method_dual(genInt, coro::ring_buffer_result::produce,
                   LibcoroRingBufferTarget, produce, int);
target_method_dual(ltest::generators::genEmpty, ConsumeValue,
                   LibcoroRingBufferTarget, consume);
target_method(ltest::generators::genEmpty, std::size_t,
              LibcoroRingBufferTarget, size);
target_method(ltest::generators::genEmpty, bool,
              LibcoroRingBufferTarget, empty);
target_method(ltest::generators::genEmpty, bool,
              LibcoroRingBufferTarget, full);
target_method(ltest::generators::genEmpty, spec::LibcoroRingBufferSnapshot,
              LibcoroRingBufferTarget, snapshot);

using spec_t = ltest::SpecDual<LibcoroRingBufferTarget, spec::LibcoroRingBuffer>;

LTEST_ENTRYPOINT_DUAL(spec_t);
