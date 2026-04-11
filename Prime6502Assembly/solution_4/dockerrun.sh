#!/bin/bash
. base.sh

XC64="x64sc"
PRG="${SRC%.asm}.prg" # Must be same as SRC with a .prg suffix
MONCMDS="mon_commands.txt"
MEMDUMP="fullmem_dump.bin"

# Vice options (beyond monitor trickery)
WARP="-warp"     # Might as well warp - the timer readings will be the same
CONS="-console"  # Run headless

# Run + analyze memdump
$XC64 $WARP $CONS -moncommands $MONCMDS -initbreak 0x0a00 $PRG > /dev/null
./analyze_memdump.py $MEMDUMP
