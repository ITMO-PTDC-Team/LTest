#include "include/fork_round.h"

#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <type_traits>

#include "include/coro_ctx_guard.h"
#include "include/shared_buffer.h"
#include "include/thread_tls.h"
#include "include/wmm/wmm.h"

namespace ltest {
namespace {
// Only controller metadata crosses the fork boundary.
struct Writer {
  std::vector<std::uint8_t> bytes;

  template <class T>
    requires(std::is_integral_v<T> || std::is_enum_v<T>)
  void Write(T value) {
    auto* data = reinterpret_cast<const std::uint8_t*>(&value);
    bytes.insert(bytes.end(), data, data + sizeof(value));
  }

  void Write(const std::string& value) {
    Write(value.size());
    bytes.insert(bytes.end(), value.begin(), value.end());
  }

  void Write(const OperationDescription& op) {
    Fields(op.id, op.builder, op.thread, op.creation_step, op.creator,
           op.progress_methods);
  }

  void Write(const EventDescription& event) {
    Fields(event.operation, event.kind, event.text);
  }

  template <class T>
  void Write(const std::vector<T>& values) {
    Write(values.size());
    for (const auto& value : values) Write(value);
  }

  template <class... T>
  void Fields(const T&... values) {
    (Write(values), ...);
  }
};

struct Reader {
  const std::vector<std::uint8_t>& bytes;
  size_t position = 0;

  void Check(size_t size) const {
    if (size > bytes.size() - position)
      throw std::runtime_error("truncated attempt metadata");
  }

  template <class T>
    requires(std::is_integral_v<T> || std::is_enum_v<T>)
  void Read(T& value) {
    Check(sizeof(value));
    std::memcpy(&value, bytes.data() + position, sizeof(value));
    position += sizeof(value);
  }

  void Read(std::string& value) {
    size_t size;
    Read(size);
    Check(size);
    value.assign(reinterpret_cast<const char*>(bytes.data() + position), size);
    position += size;
  }

  void Read(OperationDescription& op) {
    Fields(op.id, op.builder, op.thread, op.creation_step, op.creator,
           op.progress_methods);
  }

  void Read(EventDescription& event) {
    Fields(event.operation, event.kind, event.text);
  }

  template <class T>
  void Read(std::vector<T>& values) {
    size_t count;
    Read(count);
    Check(count);
    values.resize(count);
    for (auto& value : values) Read(value);
  }

  template <class... T>
  void Fields(T&... values) {
    (Read(values), ...);
  }

  void Finish() const {
    if (position != bytes.size())
      throw std::runtime_error("trailing attempt metadata");
  }
};

std::vector<std::uint8_t> EncodeReport(const AttemptReport& q) {
  Writer w;
  w.Fields(q.operations, q.ordering, q.events);
  return std::move(w.bytes);
}

AttemptReport DecodeReport(const std::vector<std::uint8_t>& bytes) {
  Reader r{bytes};
  AttemptReport q;
  r.Fields(q.operations, q.ordering, q.events);
  r.Finish();
  return q;
}

void WriteReport(Buffer& buffer, const AttemptReport& report) {
  auto bytes = EncodeReport(report);
  buffer.Reserve(bytes.size());
  std::memcpy(buffer.mapping, bytes.data(), bytes.size());
}

AttemptReport ReadReport(const Buffer& buffer) {
  auto* begin = static_cast<std::uint8_t*>(buffer.mapping);
  return DecodeReport({begin, begin + buffer.capacity});
}

AttemptReport Fail(const std::string& message) {
  AttemptReport report;
  report.result = InfrastructureFailure;
  std::cerr << "LTest infrastructure failure: " << message << '\n';
  return report;
}

void FlushOutput() {
  std::cout.flush();
  std::cerr.flush();
  std::fflush(nullptr);
}

[[noreturn]] void ExitWorker(int result) {
  FlushOutput();
  _exit(result);
}

}  // namespace

AttemptReport RunForkAttempt(const AttemptRequest& request,
                             unsigned c_rand_seed,
                             const std::function<void()>& run) {
  SchedCtxGuard guard;
  try {
    Buffer buffer;
    FlushOutput();

    const pid_t child = fork();
    if (child < 0) return Fail("fork failed");

    if (!child) {
      std::srand(c_rand_seed);

      try {
        ThreadTlsSession tls_session;
        AttemptReport report;
        AttemptSession session{request, report};
        attempt_session = &session;
        if (wmm::wmm_enabled)
          wmm::ExecutionGraph::GetInstance().Seed(c_rand_seed);
        run();

        WriteReport(buffer, report);
        ExitWorker(report.result);
      } catch (const std::exception& error) {
        ExitWorker(Fail(error.what()).result);
      }
    }

    int status = 0;
    pid_t waited;
    do {
      waited = waitpid(child, &status, 0);
    } while (waited < 0 && errno == EINTR);

    if (waited < 0) {
      return Fail("waiting for child failed");
    }
    if (WIFSIGNALED(status))
      return Fail("child signal=" + std::to_string(WTERMSIG(status)));
    if (!WIFEXITED(status)) return Fail("child did not exit normally");

    struct stat info{};
    if (fstat(buffer.fd, &info)) return Fail("reading memfd size failed");

    const int result = WEXITSTATUS(status);
    AttemptReport report;
    if (info.st_size > 0) {
      buffer.Map(info.st_size);
      report = ReadReport(buffer);
    } else if (result != Success) {
      return Fail("child exited without a report");
    }
    report.result = result;
    return report;
  } catch (const std::exception& error) {
    return Fail(error.what());
  }
}

}  // namespace ltest
