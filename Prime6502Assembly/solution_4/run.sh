#!/bin/bash
set -euo pipefail

SCRIPT_DIR=${SCRIPT_DIR:-"$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"}
. "$SCRIPT_DIR/basevars.sh"

PRG_PATH="$SCRIPT_DIR/${SRC%.asm}.prg"
MONCMDS_PATH="$SCRIPT_DIR/mon_commands.txt"
MEMDUMP_PATH="$SCRIPT_DIR/fullmem_dump.bin"
ANALYZER_PATH="$SCRIPT_DIR/$ANALYZER"

# Vice options (beyond monitor trickery)
WARP="-warp"     # Might as well warp - the timer readings will be the same
CONS="-console"  # Run headless

printf 'bank ram\n' > "$MONCMDS_PATH"
printf 's "%s" 0 0000 ffff\n' "$MEMDUMP_PATH" >> "$MONCMDS_PATH"
printf 'q\n' >> "$MONCMDS_PATH"

# Run + analyze memdump
"$XC64" "$WARP" "$CONS" -moncommands "$MONCMDS_PATH" -initbreak 0x0a00 "$PRG_PATH" > /dev/null
"$ANALYZER_PATH" "$MEMDUMP_PATH"
