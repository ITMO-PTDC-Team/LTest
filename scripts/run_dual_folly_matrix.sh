vx#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_BUILD_DIR="${ROOT_DIR}/build"
if [[ -d "${ROOT_DIR}/build-folly-docker" ]]; then
  DEFAULT_BUILD_DIR="${ROOT_DIR}/build-folly-docker"
fi

BUILD_DIR="${BUILD_DIR:-${DEFAULT_BUILD_DIR}}"
CMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE:-Release}"
CMAKE_GENERATOR="${CMAKE_GENERATOR:-Ninja}"
CTEST_PARALLEL_LEVEL="${CTEST_PARALLEL_LEVEL:-4}"

if [[ ! -f "${BUILD_DIR}/CMakeCache.txt" ]]; then
  cmake -S "${ROOT_DIR}" -B "${BUILD_DIR}" \
    -G "${CMAKE_GENERATOR}" \
    -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}"
fi

cmake --build "${BUILD_DIR}" --target verify-dual-folly

ctest --test-dir "${BUILD_DIR}" \
  -L dual-folly \
  --parallel "${CTEST_PARALLEL_LEVEL}" \
  --output-on-failure \
  "$@"
