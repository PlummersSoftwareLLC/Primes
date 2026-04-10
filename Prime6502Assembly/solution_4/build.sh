#!/bin/bash
KICKASS="KickAssembler/KickAss.jar"
SRC="PrimeSieve1M.asm"
PRG="PrimeSieve1M.prg" # Must be same as SRC with a .prg suffix

# Assemble - produce BATCHMODE version of the program
java -jar $KICKASS -define BATCHMODE $SRC
