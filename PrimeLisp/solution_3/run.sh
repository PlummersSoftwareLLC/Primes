#!/bin/sh
set -eu
cd -- "$(dirname -- "$0")"

case "${1:-batch}" in
    batch|check|repl) mode=${1:-batch} ;;
    *) echo 'Usage: ./run.sh [batch|check|repl] [seconds=5] [repeats=1]' >&2; exit 2 ;;
esac
if ! grep -qw avx2 /proc/cpuinfo; then
    echo 'This solution requires Linux x86-64 with AVX2.' >&2
    exit 1
fi
mkdir -p .build
export PRIMES_SECONDS=${2:-5} PRIMES_REPEATS=${3:-1}
if [ "$mode" = repl ]; then
    exec "${SBCL:-sbcl}" --dynamic-space-size 1024 --control-stack-size 8 \
        --noinform --no-sysinit --no-userinit --load bootstrap.lisp
fi
if [ "$mode" = check ]; then
    exec "${SBCL:-sbcl}" --dynamic-space-size 1024 --control-stack-size 8 \
        --noinform --no-sysinit --no-userinit --non-interactive \
        --load bootstrap.lisp --load check.lisp
fi
exec "${SBCL:-sbcl}" --dynamic-space-size 1024 --control-stack-size 8 \
    --noinform --no-sysinit --no-userinit --non-interactive \
    --load bootstrap.lisp \
    --eval '(prime-bench:measure (parse-integer (sb-ext:posix-getenv "PRIMES_SECONDS")) (parse-integer (sb-ext:posix-getenv "PRIMES_REPEATS")))'
