#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_BUILD_DIR="${ROOT_DIR}/build"
if [[ -d "${ROOT_DIR}/build-folly-docker" ]]; then
  DEFAULT_BUILD_DIR="${ROOT_DIR}/build-folly-docker"
fi
BUILD_DIR="${BUILD_DIR:-${DEFAULT_BUILD_DIR}}"
LOG_DIR="${LOG_DIR:-/tmp/ltest_folly_matrix/manual}"
DEADLOCK_POLICY="${DEADLOCK_POLICY:-explore}"
TIMEOUT="${TIMEOUT:-240s}"
RUN_TLA="${RUN_TLA:-1}"
RUN_MINIMIZE="${RUN_MINIMIZE:-1}"
ALLOW_KNOWN_FAILURES="${ALLOW_KNOWN_FAILURES:-1}"
CONTINUE_ON_FAIL="${CONTINUE_ON_FAIL:-0}"

export LD_LIBRARY_PATH="${BUILD_DIR}/runtime:${BUILD_DIR}/codegen:${BUILD_DIR}/third_party/gflags/lib:${BUILD_DIR}/runtime/third_party/gflags/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"

PASS_TARGETS=(
  folly_coro_baton
  folly_coro_bounded_queue
  folly_coro_mutex
  folly_coro_shared_mutex
  folly_coro_shared_promise
  folly_coro_unbounded_queue
)

mkdir -p "${LOG_DIR}"

status=0

run_case() {
  local target="$1"
  local variant="$2"
  local expectation="$3"
  shift 3

  local bin="${BUILD_DIR}/verifying/dual/folly/${target}"
  local log="${LOG_DIR}/${target}_${variant}.log"

  if [[ ! -x "${bin}" ]]; then
    printf 'MISSING %-28s %s bin=%s\n' "${target}" "${variant}" "${bin}"
    return 1
  fi

  printf 'START   %-28s %s\n' "${target}" "${variant}"
  local deadlock_args=(--deadlock_policy="${DEADLOCK_POLICY}")

  timeout "${TIMEOUT}" "${bin}" "$@" "${deadlock_args[@]}" \
    >"${log}" 2>&1
  local rc=$?

  if [[ "${expectation}" == "pass" ]]; then
    if [[ ${rc} -eq 0 ]]; then
      printf 'PASS    %-28s %s\n' "${target}" "${variant}"
      return 0
    fi

    printf 'FAIL    %-28s %s rc=%s log=%s\n' \
      "${target}" "${variant}" "${rc}" "${log}"
    tail -n 80 "${log}"
    return "${rc}"
  fi

  if [[ "${expectation}" == "known_non_linearized" ]]; then
    if [[ ${rc} -ne 0 ]] && grep -q 'non linearized:' "${log}"; then
      printf '\nsuccess!\n' >>"${log}"
      printf 'KNOWN   %-28s %s non_linearized log=%s\n' \
        "${target}" "${variant}" "${log}"
      return 0
    fi
  fi

  if [[ "${expectation}" == "known_timeout" ]]; then
    if [[ ${rc} -eq 124 ]]; then
      printf '\nsuccess!\n' >>"${log}"
      printf 'KNOWN   %-28s %s timeout log=%s\n' \
        "${target}" "${variant}" "${log}"
      return 0
    fi
  fi

  printf 'FAIL    %-28s %s rc=%s log=%s\n' \
    "${target}" "${variant}" "${rc}" "${log}"
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

  try_case "${target}" random_t2 "${expectation}" \
    --strategy random --threads 2 --tasks 4 --rounds 40 --seed 291 || return 1
  try_case "${target}" pct_t2 "${expectation}" \
    --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 292 || return 1
  try_case "${target}" weighted_t2 "${expectation}" \
    --strategy random --threads 2 --weights 3,1 --tasks 4 --rounds 30 \
    --seed 293 || return 1
  try_case "${target}" rr_t2 "${expectation}" \
    --strategy rr --threads 2 --tasks 4 --rounds 30 --seed 294 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 "${expectation}" \
      --strategy tla --threads 2 --tasks 3 --switches 3 --rounds 10 \
      --seed 295 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" pct_min_t2 "${expectation}" \
      --strategy pct --threads 2 --tasks 4 --rounds 20 \
      --minimize=true --exploration_runs=2 --minimization_runs=1 \
      --seed 301 || return 1
  fi
}

run_shared_promise_matrix() {
  local target="$1"

  try_case "${target}" random_t1 pass \
    --strategy random --threads 1 --tasks 2 --rounds 5 --seed 321 || return 1
  try_case "${target}" pct_t1 pass \
    --strategy pct --threads 1 --tasks 2 --rounds 5 --seed 322 || return 1
  try_case "${target}" rr_t1 pass \
    --strategy rr --threads 1 --tasks 2 --rounds 5 --seed 323 || return 1
}

run_shared_mutex_matrix() {
  local target="$1"

  try_case "${target}" weighted_t2 pass \
    --strategy random --threads 2 --weights 3,1 --tasks 4 --rounds 20 \
    --seed 401 || return 1
  try_case "${target}" pct_t3 pass \
    --strategy pct --threads 3 --tasks 6 --rounds 20 --seed 402 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 5 --rounds 30 --seed 411 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 20 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" pct_min_t3 pass \
      --strategy pct --threads 3 --tasks 6 --rounds 20 \
      --minimize=true --exploration_runs=4 --minimization_runs=2 || return 1
  fi
}

run_bounded_queue_matrix() {
  local target="$1"

  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 4 --rounds 20 --seed 501 || return 1
  try_case "${target}" pct_t2 pass \
    --strategy pct --threads 2 --tasks 2 --rounds 5 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 4 --rounds 10 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 4 --switches 4 --rounds 5 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" random_min_t2 pass \
      --strategy random --threads 2 --tasks 4 --rounds 15 \
      --minimize=true --exploration_runs=3 --minimization_runs=2 || return 1
  fi
}

run_mutex_matrix() {
  local target="$1"

  try_case "${target}" random_t2 pass \
    --strategy random --threads 2 --tasks 4 --rounds 30 --seed 311 || return 1
  try_case "${target}" pct_t2 pass \
    --strategy pct --threads 2 --tasks 4 --rounds 30 --seed 312 || return 1
  try_case "${target}" rr_t2 pass \
    --strategy rr --threads 2 --tasks 4 --rounds 20 --seed 313 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 5 --switches 4 --rounds 20 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" pct_min_t2 pass \
      --strategy pct --threads 2 --tasks 4 --rounds 15 \
      --minimize=true --exploration_runs=2 --minimization_runs=1 \
      --seed 314 || return 1
  fi
}

run_unbounded_queue_matrix() {
  local target="$1"

  try_case "${target}" weighted_random_t2 pass \
    --strategy random --threads 2 --weights 5,1 --tasks 2 --rounds 5 || return 1
  try_case "${target}" random_t1 pass \
    --strategy random --threads 1 --tasks 2 --rounds 10 || return 1
  try_case "${target}" rr_t1 pass \
    --strategy rr --threads 1 --tasks 2 --rounds 10 || return 1

  if [[ "${RUN_TLA}" == "1" ]]; then
    try_case "${target}" tla_t2 pass \
      --strategy tla --threads 2 --tasks 2 --switches 4 --rounds 10 || return 1
  fi

  if [[ "${RUN_MINIMIZE}" == "1" ]]; then
    try_case "${target}" tla_min_t2 pass \
      --strategy tla --threads 2 --tasks 2 --switches 4 --rounds 5 \
      --minimize=true --exploration_runs=3 --minimization_runs=2 || return 1
  fi
}

for target in "${PASS_TARGETS[@]}"; do
  if [[ "${target}" == "folly_coro_shared_promise" ]]; then
    run_shared_promise_matrix "${target}" || status=1
  elif [[ "${target}" == "folly_coro_bounded_queue" ]]; then
    run_bounded_queue_matrix "${target}" || status=1
  elif [[ "${target}" == "folly_coro_mutex" ]]; then
    run_mutex_matrix "${target}" || status=1
  elif [[ "${target}" == "folly_coro_shared_mutex" ]]; then
    run_shared_mutex_matrix "${target}" || status=1
  elif [[ "${target}" == "folly_coro_unbounded_queue" ]]; then
    run_unbounded_queue_matrix "${target}" || status=1
  else
    run_target_matrix "${target}" pass || status=1
  fi
done

logs=$(find "${LOG_DIR}" -maxdepth 1 -name '*.log' | wc -l)
bad=$(grep -L 'success!' "${LOG_DIR}"/*.log 2>/dev/null | wc -l)
printf 'SUMMARY logs=%s bad=%s log_dir=%s run_tla=%s run_minimize=%s allow_known_failures=%s\n' \
  "${logs}" "${bad}" "${LOG_DIR}" "${RUN_TLA}" "${RUN_MINIMIZE}" "${ALLOW_KNOWN_FAILURES}"

if [[ ${status} -ne 0 ]]; then
  grep -L 'success!' "${LOG_DIR}"/*.log 2>/dev/null || true
fi

exit "${status}"
