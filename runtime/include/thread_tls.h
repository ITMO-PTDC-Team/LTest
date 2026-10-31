#pragma once

#include <cstddef>
#include <functional>

namespace ltest {

class DonorTls;

void* CurrentTlsBase() noexcept;

// Donor storage is created on first use in a forked worker and lives until
// child _exit, including records still used by parked native threads.
DonorTls& GetLogicalThreadTls(std::size_t thread);
void RunWithLogicalThreadTls(std::size_t thread,
                             const std::function<void()>& function);

}  // namespace ltest
