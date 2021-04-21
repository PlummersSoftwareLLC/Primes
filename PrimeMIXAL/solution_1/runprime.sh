#!/bin/bash
`which time` -f '%e' mixvm -r prime | tr '[:upper:]' '[:lower:]' | sed -n "s/<time>/`cat \/tmp\/primetime.txt`/p"
