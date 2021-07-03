#!/bin/bash
[[ -z "$LATEX" ]] && LATEX=latex
cd "$(dirname $0)"
rm *.aux *.log
"$LATEX" -halt-on-error prime_race.tex >/dev/null && cat result.txt
