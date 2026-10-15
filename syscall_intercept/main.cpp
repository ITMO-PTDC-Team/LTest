#include <map>

#include "runtime/include/value_wrapper.h"
#include "runtime/include/verifying.h"
using namespace ltest;
namespace spec {

struct FireAndForgetOptionsOverride {
  static ltest::DefaultOptions GetOptions() {
    return {.threads = 1,
            .tasks = 100000000,
            .switches = 100000000,
            .rounds = 10000,
            .verbose = false,
            .strategy = "pct",
            .weights = ""};
  }
};

static int test_argc;
static char **test_argv;

std::tuple<int, char **> genMainArgs(size_t thread_num) {
  assert(thread_num == 0);
  return {test_argc, test_argv};
}

}  // namespace spec

extern "C" int test_main(int argc, char *argv[]);

class Test {
 public:
  int Main(int argc, char *argv[]) {
    const int result = test_main(argc, argv);
    if (result != 0) {
      SchedCtxGuard guard;
      SetTestFailure("target main returned " + std::to_string(result));
    }
    return result;
  }
};

target_method(spec::genMainArgs, int, Test, Main, int, char **);

class DummyChecker : public ModelChecker {
 public:
  bool Check(const std::vector<HistoryEvent> &) override { return true; }
};
extern "C" int ltest_main(int argc, char *argv[]) {
  ltest_initialized = true;
  SetOpts(spec::FireAndForgetOptionsOverride::GetOptions());
  spec::test_argc = argc;
  spec::test_argv = argv;
  Opts opts = ParseOpts();

  logger_init(opts.verbose);

  auto strategy = MakeStrategy<Test, OnlyOneTaskPerThreadVerifier>(
      opts, task_builders, []() { return std::make_unique<Test>(); });
  auto run_once = [&](bool) {
    PrettyPrinter pretty_printer;
    DummyChecker checker;
    auto scheduler = MakeScheduler<OnlyOneTaskPerThreadVerifier>(
        checker, opts, *strategy, false, pretty_printer);
    std::cout << "\n\n";
    std::cout.flush();
    TrapRun(*scheduler, pretty_printer);
  };
  return RunStrategyRounds(opts, *strategy, run_once);
}
