#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
LCI_BIN="$SCRIPT_DIR/.build/prefix/bin/lci"
if [ ! -x "$LCI_BIN" ] && [ -x /opt/lci/bin/lci ]; then
  LCI_BIN=/opt/lci/bin/lci
fi

cd "$SCRIPT_DIR"

if [ ! -x "$LCI_BIN" ]; then
  ./build.sh >&2
fi

exec "$LCI_BIN" ./lolprime.lol
