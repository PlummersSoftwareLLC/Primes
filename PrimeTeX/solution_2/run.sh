#!/bin/sh
cd "$(dirname $0)"
rm -f erato_benchmark-out.txt
rm -f wheel_benchmark-out.txt
luatex erato_benchmark.tex >/dev/null && cat erato_benchmark-out.txt
luatex wheel_benchmark.tex >/dev/null && cat wheel_benchmark-out.txt
