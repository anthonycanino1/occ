#!/usr/bin/env bash

# Copyright 2016 The occ Authors. All rights reserved.

if [ ! -f check.rb ]; then
  echo "test.bash must be run from $OCCROOT/test" 1>&2
  exit 1
fi

if ! hash occ 2>/dev/null; then
  echo "occ not found in path"
  exit 1
fi

fails=0
passed=0
for f in *.c; do
  result=$(./check.rb $f)
  status=$?
  if [[ $status != 0 ]]; then
    printf "FAIL %10s\n" $f
    printf "exit status %d\n" $status
    printf "%s\n" "$result"
    fails=$(($fails+1))
  else
    printf "OK %10s\n" $f
    passed=$(($passed+1))
  fi
done

echo "testing completed with $passed passes and $fails failures"
if [[ $fails != 0 ]]; then
  exit 1
else
  exit 0
fi
