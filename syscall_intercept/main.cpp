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
  int Main(int argc, char *argv[]) { return test_main(argc, argv); };
};

target_method(spec::genMainArgs, int, Test, Main, int, char **);

class DummyChecker : public ModelChecker {
 public:
  bool Check(const std::vector<HistoryEvent> &) override { return true; }
};
extern "C" int ltest_main(int argc, char *argv[]) {
  ltest_initialized = true;
  spec::test_argc = argc;
  spec::test_argv = argv;
  SetOpts(spec::FireAndForgetOptionsOverride::GetOptions());
  Opts opts = ParseOpts();

  logger_init(opts.verbose);

  PrettyPrinter pretty_printer;
  DummyChecker checker;
  auto scheduler = MakeScheduler<Test, OnlyOneTaskPerThreadVerifier>(
      checker, opts, task_builders, {}, pretty_printer,  [](){return std::make_unique<Test>();});
  std::cout << "\n\n";
  std::cout.flush();
  return TrapRun(std::move(scheduler), pretty_printer);
}
