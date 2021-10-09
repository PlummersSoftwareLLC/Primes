#!/bin/bash
`which time` -f '%e' mixvm -r prime 2> /tmp/primetime.txt | tr '[:upper:]' '[:lower:]' > /tmp/primeoutput.txt
sed -n "s/<time>/`cat \/tmp\/primetime.txt`/p" /tmp/primeoutput.txt
