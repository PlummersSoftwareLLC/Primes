#!/bin/bash
[[ -z "$LATEX" ]] && LATEX=latex
cd "$(dirname $0)"
rm -f *.aux *.log 2>&1
"$LATEX" -halt-on-error prime_race.tex >/dev/null && cat result.txt
