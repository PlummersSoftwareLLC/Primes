#! /bin/bash
#
# A simple prime sieve based on https://github.com/davepl/Primes
# Written in bash.
#
# PrimeSH.sh
# Copyright (C) 2021 nitepone <admin@night.horse>
#
# Distributed under terms of the MIT license.
#


declare -A primeCounts=( \
	[10]=1
	[100]=25
	[1000]=168
	[10000]=1229
	[100000]=9592
	[1000000]=78498
	[10000000]=664579
	[100000000]=576145
)

sieveSize=0
bitArray=()
RUNTIME_SEC=10



function pr {
	printf "\\e[36m[PrimeBash]\\e[0m "
	printf "%s\\n" "$@"
}

function pr_err {
	printf "\\e[36m[PrimeBash]\\e[31m " >&2
	printf "%s\\n" "$@" >&2
	printf "\\e[0m" >&2
}



function init_globals {
	sieveSize=$1
	for ((i=0; i<((sieveSize+1) / 2); i++)); do
		bitArray[$i]=1
	done

}

function validateResults {
	result=$1
	return $(( primeCounts["$sieveSize"] == result ))
}

function countPrimes {
	local count
	count=0
	for i in "${!bitArray[@]}"; do
		if (( "${bitArray[i]}" == 1 )); then
			((count++))
		fi
	done
	echo "$count"
}

function getBit {
	local index
	index=$1
	if (( index % 2 == 0 )); then
		return 1
	fi
	return "${bitArray[$((index/2))]}"
}

function clearBit {
	local index
	index=$1
	if (( index % 2 == 0 )); then
		pr_err "You are setting even bits, which is sub-optimal"
		return
	fi
	bitArray[$((index/2))]=0
}

function runSieve {
	local factor
	local q
	factor=3
	q=$(awk "END{print int(sqrt($sieveSize))}" </dev/null)
	while ((factor <= q)); do
		for ((num=factor; num<=sieveSize; num+=2)); do
			if ! getBit "$num"; then
				((factor=num))
				break
			fi
		done
		for ((num=(factor*factor); num<=sieveSize; num+=factor*2)); do
			clearBit "$num"
		done
		((factor+=2))
	done
}

function printResults {
	local showresults
	local duration
	local passes
	local count
	local valid
	showresults="$1"
	duration_nano="$2"
	duration_mill="$((duration_nano/1000000))"
	passes="$3"
	validateResults "$(countPrimes)"
	valid="$?"

	if ((showresults)); then
		printf "2, "
	fi
	count=1

	for ((num=3; num<sieveSize; num+=2)); do
		if ! getBit "$num"; then
			if ((showresults)); then
				printf "%s, " "$num"
			fi
			((count++))
		fi
	done

	if ((count != $(countPrimes))); then
		pr_err "Internal: Print Results Counted Incorrectly..."
	fi
	printf "\n"
	printf "Passes: %s, Time: %s, Avg(ms): %s, Limit %s, Count: %s, Valid: %s\n" \
		"$passes" "$duration_mill" "$((duration_mill/passes))" "$sieveSize" \
		"$count" "$valid"

}



# we are working in nanoseconds (10^9)
tDiff=$((1000000000*RUNTIME_SEC))
passes=0
tStart=$(date +%s%N)
while (( $(date +%s%N) < (tDiff + tStart) )); do
	init_globals "1000000"
	runSieve
	((passes++))
done

# calculate real runtime
tRun=$(($(date +%s%N) - tStart))

printResults "0" "$tRun" "$passes"
