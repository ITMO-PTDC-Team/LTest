#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_BUILD_DIR="${ROOT_DIR}/build"
if [[ -d "${ROOT_DIR}/build-libcoro-docker" ]]; then
  DEFAULT_BUILD_DIR="${ROOT_DIR}/build-libcoro-docker"
fi
BUILD_DIR="${BUILD_DIR:-${DEFAULT_BUILD_DIR}}"
LOG_DIR="${LOG_DIR:-/tmp/ltest_libcoro_matrix/manual}"
DEADLOCK_POLICY="${DEADLOCK_POLICY:-explore}"
TIMEOUT="${TIMEOUT:-180s}"
ALLOW_KNOWN_FAILURES="${ALLOW_KNOWN_FAILURES:-1}"
RUN_TLA="${RUN_TLA:-1}"
RUN_MINIMIZE="${RUN_MINIMIZE:-1}"
CONTINUE_ON_FAIL="${CONTINUE_ON_FAIL:-0}"

export LD_LIBRARY_PATH="${BUILD_DIR}/runtime:${BUILD_DIR}/codegen:${BUILD_DIR}/third_party/gflags/lib:${BUILD_DIR}/runtime/third_party/gflags/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"

STABLE_TARGETS=(
  libcoro_event
  libcoro_latch
  libcoro_mutex
  libcoro_queue
  libcoro_ring_buffer
  libcoro_semaphore
  libcoro_shared_mutex
)

KNOWN_FAILING_TARGETS=(
)

mkdir -p "${LOG_DIR}"

status=0

run_case() {
  local target="$1"
  local variant="$2"
  local expectation="$3"
  shift 3

  local bin="${BUILD_DIR}/verifying/dual/libcoro/${target}"
  local log="${LOG_DIR}/${target}_${variant}.log"

  if [[ ! -x "${bin}" ]]; then
    printf 'MISSING    %-24s %s bin=%s\n' "${target}" "${variant}" "${bin}"
    return 1
  fi

  printf 'START      %-24s %s\n' "${target}" "${variant}"
  local deadlock_args=(--deadlock_policy="${DEADLOCK_POLICY}")

  timeout "${TIMEOUT}" "${bin}" "$@" "${deadlock_args[@]}" \
    >"${log}" 2>&1
  local rc=$?

  if [[ "${expectation}" == "pass" ]]; then
    if [[ ${rc} -eq 0 ]]; then
      printf 'PASS       %-24s %s\n' "${target}" "${variant}"
      return 0
    fi

    printf 'FAIL       %-24s %s rc=%s log=%s\n' "${target}" "${variant}" "${rc}" "${log}"
    tail -n 80 "${log}"
    return "${rc}"
  fi

  if [[ ${rc} -eq 0 ]]; then
    printf 'UNEXPECTED %-24s %s known failure passed\n' "${target}" "${variant}"
    return 1
  fi

  if grep -q 'non linearized:' "${log}"; then
    printf 'KNOWN_FAIL %-24s %s rc=%s log=%s\n' "${target}" "${variant}" "${rc}" "${log}"
    return 0
  fi

  printf 'FAIL       %-24s %s rc=%s log=%s\n' "${target}" "${variant}" "${rc}" "${log}"
  tail -n 80 "${log}"
  return "${rc}"
}

try_case() {
  if run_case "$@"; then
    return 0
  fi

  if [[ "${CONTINUE_ON_FAIL}" == "1" ]]; then
    status=1
    return 0
  fi

  return 1
}

run_target_matrix() {
  local target="$1"
  local expectation="$2"

  try_case "${target}" random_t3 "${expectation}" \
    --strategy random --threads 3 --tasks 8 --rounds 60 || return 1
  try_case "${target}" pct_t3 "${expectation}" \
    --strategy pct --threads 3 --tasks 8 --rounds 40 || return 1
  try_case "${target}" weighted_t3 "${expectation}" \
    --strategy random --threads 3 --weights 3,1,2 --tasks 8 --rounds 40 || return 1
  try_case "${target}" rr_t2 "${expectation}" \
    --strategy rr --threads 2 --tasks 6 --rounds 30 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 "${expectation}" \
      --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 15 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" pct_min_t3 "${expectation}" \
      --strategy pct --threads 3 --tasks 8 --rounds 30 \
      --minimize=true --exploration_runs=4 --minimization_runs=2 || return 1
  fi
}

run_shared_mutex_matrix() {
  local target="$1"

  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 4 --rounds 20 || return 1
  try_case "${target}" weighted_t2 pass \
    --strategy random --threads 2 --weights 3,1 --tasks 4 --rounds 20 || return 1
  try_case "${target}" random_t3 pass \
    --strategy random --threads 3 --tasks 6 --rounds 20 || return 1
  try_case "${target}" weighted_t3 pass \
    --strategy random --threads 3 --weights 3,1,2 --tasks 6 --rounds 20 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 6 --rounds 30 || return 1
}

run_mutex_matrix() {
  local target="$1"

  if [[ "${DEADLOCK_POLICY}" == "rollback" ||
        "${DEADLOCK_POLICY}" == "fail" ]]; then
    try_case "${target}" rr_t2 pass \
      --strategy rr --threads 2 --tasks 4 --rounds 20 --seed 204 || return 1
    return 0
  fi

  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 4 --rounds 40 --seed 203 || return 1
  try_case "${target}" pct_t2 pass \
    --strategy pct --threads 2 --tasks 5 --rounds 20 --seed 202 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 4 --rounds 20 --seed 204 || return 1

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" random_min_t2 pass \
      --strategy random --threads 2 --tasks 4 --rounds 15 \
      --minimize=true --exploration_runs=2 --minimization_runs=1 \
      --seed 206 || return 1
  fi
}

run_queue_matrix() {
  local target="$1"

  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 4 --rounds 40 --seed 111 || return 1
  try_case "${target}" pct_t2 pass \
    --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 112 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 4 --rounds 30 --seed 113 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 2 --switches 2 --rounds 5 \
      --seed 114 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" pct_min_t2 pass \
      --strategy pct --threads 2 --tasks 4 --rounds 15 \
      --minimize=true --exploration_runs=2 --minimization_runs=1 \
      --seed 115 || return 1
  fi
}

run_semaphore_matrix() {
  local target="$1"

  try_case "${target}" random_t3 pass \
    --strategy random --threads 3 --tasks 8 --rounds 60 || return 1
  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 6 --rounds 40 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 6 --rounds 40 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 20 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" random_min_t3 pass \
      --strategy random --threads 3 --tasks 8 --rounds 30 \
      --minimize=true --exploration_runs=4 --minimization_runs=2 || return 1
  fi
}

run_ring_buffer_matrix() {
  local target="$1"

  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 4 --rounds 40 --seed 121 || return 1
  try_case "${target}" pct_t2 pass \
    --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 122 || return 1
  try_case "${target}" weighted_t2 pass \
    --strategy random --threads 2 --weights 3,1 --tasks 4 --rounds 20 \
    --seed 123 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 4 --rounds 30 --seed 124 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 2 --switches 2 --rounds 5 \
      --seed 125 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" pct_min_t2 pass \
      --strategy pct --threads 2 --tasks 4 --rounds 15 \
      --minimize=true --exploration_runs=2 --minimization_runs=1 \
      --seed 126 || return 1
  fi
}

for target in "${STABLE_TARGETS[@]}"; do
  if [[ "${target}" == "libcoro_shared_mutex" ]]; then
    run_shared_mutex_matrix "${target}" || status=1
  elif [[ "${target}" == "libcoro_mutex" ]]; then
    run_mutex_matrix "${target}" || status=1
  elif [[ "${target}" == "libcoro_queue" ]]; then
    run_queue_matrix "${target}" || status=1
  elif [[ "${target}" == "libcoro_semaphore" ]]; then
    run_semaphore_matrix "${target}" || status=1
  elif [[ "${target}" == "libcoro_ring_buffer" ]]; then
    run_ring_buffer_matrix "${target}" || status=1
  else
    run_target_matrix "${target}" pass || status=1
  fi
done

if [[ "${INCLUDE_KNOWN_FAILURES:-1}" == "1" ]]; then
  for target in "${KNOWN_FAILING_TARGETS[@]}"; do
    if [[ "${ALLOW_KNOWN_FAILURES}" == "1" ]]; then
      run_case "${target}" minimal_known_failure known_failure \
        --strategy random --threads 2 --tasks 3 --rounds 50 || status=1
    else
      run_target_matrix "${target}" pass || status=1
    fi
  done
fi

logs=$(find "${LOG_DIR}" -maxdepth 1 -name '*.log' | wc -l)
bad=$(grep -L 'success!' "${LOG_DIR}"/*.log 2>/dev/null | wc -l)
printf 'SUMMARY logs=%s bad=%s log_dir=%s allow_known_failures=%s\n' \
  "${logs}" "${bad}" "${LOG_DIR}" "${ALLOW_KNOWN_FAILURES}"

if [[ ${bad} -ne 0 ]]; then
  grep -L 'success!' "${LOG_DIR}"/*.log 2>/dev/null
fi

exit "${status}"
