#!/usr/bin/env bash

exp_code=$1
shift 1
LD_PRELOAD=./build/syscall_intercept/libpreload.so $@
if [[ $? -ne $exp_code ]]; then
    exit 1
fi