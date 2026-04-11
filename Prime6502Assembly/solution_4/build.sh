#!/bin/bash
. base.sh

KICKASS="KickAssembler/KickAss.jar"

# Assemble - produce BATCHMODE version of the program
java -jar $KICKASS -define BATCHMODE $SRC
