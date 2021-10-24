#!/bin/sh

SLEEP=5

run() {
    ./zig-out/bin/PrimeZig -l "$1"
    sleep "$SLEEP"
}

for LINE in 44 45 47 49 75 81 83 85 86 87 88 89 90 91 92 93; do
  run "$LINE"
done
