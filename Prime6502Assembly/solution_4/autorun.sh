#!/bin/bash
export SCRIPT_DIR=${SCRIPT_DIR:-"$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"}

"$SCRIPT_DIR/build.sh"
"$SCRIPT_DIR/run.sh"
