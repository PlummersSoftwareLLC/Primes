#! /bin/bash
#
# A simple prime sieve based on https://github.com/davepl/Primes
# Written in bash.
#
# Tyler Hart (nitepone) <admin@night.horse>

# rbergen -- changed number for 10 from 1 to 4
readonly -A primeCounts=(
	[10]=4
	[100]=25
	[1000]=168
	[10000]=1229
	[100000]=9592
	[1000000]=78498
	[10000000]=664579
	[100000000]=576145
)

declare sieveSize=0
declare -A bitArray=()
# rbergen: changed runtime to drag-race default
readonly RUNTIME_SEC=5

function sqrt {
	# integer square root using binary search (lilweege)
	local -n lo=$1 # Output
	lo=1
	hi=$2
	mid=0
	while ((lo<=hi)); do
		((mid=(lo+hi)/2))
		if [[ $((mid*mid)) -le $2 ]]; then
			((lo=mid+1))
		else
			((hi=mid-1))
		fi
	done
	((--lo))
}

function initGlobals {
	sieveSize=$1
}

function emptyBitArray {
	bitArray=()
}

function validateResults {
	local result=$1
	(( primeCounts[\$sieveSize] == result ))
}

function countPrimes {
	local i count=$((sieveSize >= 2))
	for ((i=3; i < sieveSize; i++)); do
		if getBit "$i"; then
			((++count))
		fi
	done
	echo "$count"
}

function getBit {
	# Return failure if even if bitArray is set (determined composite)
	# Return success if known prime
	(( $1 & 1 && !bitArray[\$1] ))
}

function clearBit {
	# Mark as composite
	bitArray[$1]=1
}

function runSieve {
	local factor num q
	sqrt q "$sieveSize"
	for ((factor=3; factor <= q; factor+=2)); do
		for ((num=factor; num < sieveSize; num+=2)); do
			if getBit "$num"; then
				factor=$num
				break
			fi
		done
		for ((num=factor**2; num < sieveSize; num+=factor*2)); do
			clearBit "$num"
		done
	done
}

function printResults {
	local showresults dur_usec avg_dur_usec dur_str avg_dur_str passes count valid
	showresults="$1"
	dur_usec="$2"
	passes="$3"
	# create duration strings from millisecond duration time
	avg_dur_usec=$[dur_usec/passes]
	printf -v dur_str "%d.%06d" \
		"$[dur_usec/1000000]" "$[dur_usec%1000000]"
	printf -v avg_dur_str "%d.%06d" \
		"$[avg_dur_usec/1000000]" "$[avg_dur_usec%1000000]"
	# create validity string
	if validateResults "$(countPrimes)"; then
		valid="True"
	else
		valid="False"
	fi

	if ((showresults)); then
		printf "2, "
	fi

	count=$((sieveSize >= 2))
	for ((num=3; num<sieveSize; num+=2)); do
		if getBit "$num"; then
			if ((showresults)); then
				printf "%s, " "$num"
			fi
			((count++))
		fi
	done

	if ((count != $(countPrimes))); then
		echo "Internal: Print Results Counted Incorrectly..." >&2
		exit 1
	fi
	printf "\n"
	printf "Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s\n" \
		"$passes" "$dur_str" "$avg_dur_str" "$sieveSize" \
		"$count" "$valid"

	# rbergen: added drag-race format output
	printf "\nbash;%s;%s;1;algorithm=base,faithful=no\n" "$passes" "$dur_str"
}

function main {
	export LC_ALL=C
	local passes=0 tStart tStop tNow tRun

	# Keep /dev/null handle open so we don't pay for opening it later
	exec 3>/dev/null

	initGlobals "1000000"

	tStart="${EPOCHREALTIME/.}"
	# we are working in microseconds (10^6)
	tStop="$[tStart + RUNTIME_SEC*1000000]"

	tNow="${EPOCHREALTIME/.}"
	while [ $tNow -lt $tStop ]
	do
		emptyBitArray
		runSieve
		((++passes))
		tNow="${EPOCHREALTIME/.}"
	done

	# calculate real runtime
	tRun=$[tNow - tStart]

	printResults "0" "$tRun" "$passes"
}

main
