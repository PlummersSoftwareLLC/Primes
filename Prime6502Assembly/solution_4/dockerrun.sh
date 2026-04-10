#!/bin/bash
XC64="x64sc"
PRG="PrimeSieve1M.prg" # Must be same as SRC with a .prg suffix
MONCMDS="mon_commands.txt"
MEMDUMP="fullmem_dump.bin"

# Vice options (beyond monitor trickery)
WARP="-warp"     # Might as well warp - the timer readings will be the same
CONS="-console"  # Run headless

# Construct mon command file - to be triggered at the data dump hook at $0a00
echo 'bank ram' > $MONCMDS
echo 's "'$MEMDUMP'" 0 0000 ffff' >> $MONCMDS
echo 'q' >> $MONCMDS

# Run + analyze memdump
$XC64 $WARP $CONS -moncommands $MONCMDS -initbreak 0x0a00 $PRG > /dev/null
./analyze_memdump.py $MEMDUMP
