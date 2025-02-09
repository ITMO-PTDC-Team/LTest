#pragma once
#include <iostream>

#ifdef DEBUG
#define debug(...) fprintf(__VA_ARGS__)
#else
#define debug(...)
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

  void flush();
};

void logger_init(bool verbose);

Logger& log();
