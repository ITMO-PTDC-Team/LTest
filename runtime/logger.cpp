#include "include/logger.h"

#include <iostream>

Logger l{};

void LoggerInit(bool verbose) { l.verbose = verbose; }

void Logger::Flush() {
  if (verbose) {
    std::cout.flush();
  }
}

Logger& Log() { return l; }
