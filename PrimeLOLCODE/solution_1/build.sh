#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD_ROOT="$SCRIPT_DIR/.build"
SRC_DIR="$BUILD_ROOT/lci"
BUILD_DIR="$BUILD_ROOT/cmake"
PREFIX_DIR="$BUILD_ROOT/prefix"
LCI_REPO="https://github.com/justinmeza/lci.git"
LCI_COMMIT="9377c404c79a122a4698d98118eef44310c751be"

mkdir -p "$BUILD_ROOT"

if [ ! -d "$SRC_DIR/.git" ]; then
  git clone "$LCI_REPO" "$SRC_DIR" >&2
fi

git -C "$SRC_DIR" fetch --depth 1 origin "$LCI_COMMIT" >&2
git -C "$SRC_DIR" checkout --force --detach "$LCI_COMMIT" >&2
patch -d "$SRC_DIR" -p1 < "$SCRIPT_DIR/lci-clock.patch" >&2

cmake -S "$SRC_DIR" -B "$BUILD_DIR" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$PREFIX_DIR" >&2
cmake --build "$BUILD_DIR" --parallel >&2
cmake --install "$BUILD_DIR" >&2
