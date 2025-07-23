# Ltest

LTest (Linearizability Test) is a framework for writing tests for concurrent data structures. It allows you to write complex tests in a declarative way.

LTest instruments your data structure’s methods with context switches, executes these methods in a single thread, and produces different interleavings due to the context switches. Later, the obtained execution is checked for linearizability.

Test examples can be found in the verifying directory.

## Build

It is not recommended to try install all required dependencies locally in your system. Build docker image and run container:
```sh
./scripts/rund.sh
```

## Run
* Run for release:
```sh
cmake -G Ninja -B build -DCMAKE_BUILD_TYPE=Release
```

* Run unit tests:
```sh
cmake --build build --target lin_check_test
```

* Run verify:
```sh
cmake --build build --target verifying/targets/nonlinear_queue && ./build/verifying/targets/nonlinear_queue --tasks 10 --rounds 240 --strategy pct
```
## Blocking
Verifying of blocking data structures uses syscall interception, so we need to build and install special hooks, that are required to be load through LD_PRELOAD:
```sh
cmake --build build --target verifying/blocking/nonlinear_mutex && LD_PRELOAD=build/syscall_intercept/libpreload.so ./build/verifying/blocking/nonlinear_mutex
```

Some blocking targets depends on boost and folly. For them you need to install boost and folly locally, and provide ./boost and ./folly symbolic links at the root of the project, then we can lincheck these targets.
