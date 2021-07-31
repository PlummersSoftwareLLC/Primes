#!/bin/sh
cd "$(dirname $0)"
rm -f erato_benchmark-out.txt
rm -f wheel_benchmark-out.txt
export TEXMFCNF="$(pwd):"
pdftex erato_benchmark.tex >/dev/null && cat erato_benchmark-out.txt
pdftex wheel_benchmark.tex >/dev/null && cat wheel_benchmark-out.txt
