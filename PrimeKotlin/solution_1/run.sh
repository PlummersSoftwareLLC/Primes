#!/usr/bin/env bash
set -e
export PATH=/bin/node/bin:$PATH
java -jar PrimeSieve-1.0-all.jar -t -if -i -t:m -if:m -i:m
node PrimeSieve.js -t -if -i
# Kotlin/Native doesn't work on ARM for Linux
if [[ "$(arch)" =~ "^arm*" ]]; then
	./PrimeSieve.kexe -t -if -i -t:m -if:m -i:m
fi