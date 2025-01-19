#!/usr/bin/env bash
set -e
dir=build-bench
baseline=$1
curr=$(git rev-parse --abbrev-ref HEAD)
args="--tasks 1000 --rounds 2000 --strategy random --threads 250 --verbose false --switches 100"
cmake -G Ninja -B $dir -DCMAKE_BUILD_TYPE=Release 
cmake --build $dir --target verifying/targets/atomic_register
echo "impl from $curr branch"
time ./$dir/verifying/targets/atomic_register $args
git stash
git checkout $baseline
cmake -G Ninja -B $dir -DCMAKE_BUILD_TYPE=Release >/dev/null 2>&1
cmake --build $dir --target verifying/targets/atomic_register >/dev/null 2>&1
echo "impl from $baseline commit"
time ./$dir/verifying/targets/atomic_register $args
rm -rf $dir
git switch $curr
git stash pop