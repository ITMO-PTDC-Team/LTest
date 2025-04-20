#pragma once
#include <iostream>

#ifdef DEBUG
#define DEBUG(...) fprintf(__VA_ARGS__)
#else
#define DEBUG(...)
#endif

struct Logger {
  bool verbose{};

  template <typename T>
  Logger& operator<<(const T& val) {
    if (verbose) {
      std::cout << val;
    }
    return *this;
  }

  void Flush();
};

void LoggerInit(bool verbose);

Logger& Log();
