#include "../../specs/libcoro/queue.h"

#include "../../../runtime/include/verifying.h"

#include <coro/queue.hpp>
#include <coro/task.hpp>
#include <coro/expected.hpp>

static auto genInt(size_t) {
  return ltest::generators::makeSingleArg(rand() % 10 + 1);
}

using Q = coro::queue<int>;

using PushRet = coro::task<coro::queue_produce_result>;
using PopRet  = coro::task<tl::expected<int, coro::queue_consume_result>>;

static PushRet (Q::*push_ptr)(const int&) =
    static_cast<PushRet (Q::*)(const int&)>(&Q::push);

static PopRet (Q::*pop_ptr)() =
    static_cast<PopRet (Q::*)()>(&Q::pop);

ltest::TargetDualMethod<coro::queue_produce_result, Q, int>
    push_ltest_dual_method_cls{"push", genInt, push_ptr};

ltest::TargetDualMethod<tl::expected<int, coro::queue_consume_result>, Q>
    pop_ltest_dual_method_cls{"pop", ltest::generators::genEmpty, pop_ptr};

using spec_t = ltest::SpecDual<Q, spec::LibcoroQueue>;
LTEST_ENTRYPOINT_DUAL(spec_t);
