#!/bin/bash
SCRIPT_DIR=${SCRIPT_DIR:-"$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"}
. "$SCRIPT_DIR/basevars.sh"

KICKASS_PATH="$SCRIPT_DIR/$KICKASS"
SRC_PATH="$SCRIPT_DIR/$SRC"

# Assemble - produce BATCHMODE version of the program
java -jar "$KICKASS_PATH" -define BATCHMODE "$SRC_PATH"
