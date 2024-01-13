#!/bin/bash
ARCH="$(uname -m)"
case "${ARCH}" in
    x86_64) ./Primes/build/Release_Linux64/Primes/Primes "$@"
    ;;
*)
    echo "Unknown architecture ${ARCH}"
    ;;
esac
