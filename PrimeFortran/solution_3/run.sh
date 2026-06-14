#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$SCRIPT_DIR"

if [ ! -x ./cwager_fortran_mt ]; then
  ./build.sh >&2
fi

exec ./cwager_fortran_mt
