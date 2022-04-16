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
	local index=$1 bit
	(( index & 1 && !(bit=1<<(index>>1&63), index>>=7, bitArray[\$index] & bit) ))
}

function clearBit {
	local index=$1 bit
	(( index & 1 && (bit=1<<(index>>1&63), index>>=7, bitArray[\$index] |= bit) ))
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
	local showresults dur_nano avg_dur_nano dur_str avg_dur_str passes count valid
	showresults="$1"
	dur_nano="$2"
	passes="$3"
	# create duration strings from nanosecond duration time
	avg_dur_nano=$((dur_nano/passes))
	dur_str="$(printf "%d.%09d"\
		"$((dur_nano/1000000000))"\
		"$((dur_nano%1000000000))"\
	)"
	avg_dur_str="$(printf "%d.%09d"\
		"$((avg_dur_nano/1000000000))"\
		"$((avg_dur_nano%1000000000))"\
	)"
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
	printf "\nbash_packed;%s;%s;1;algorithm=base,faithful=no\n" "$passes" "$dur_str"
}

function main {
	export LC_ALL=C
	local passes=0 sleepPid tStart tRun

	# Keep /dev/null handle open so we don't pay for opening it later
	exec 3>/dev/null

	initGlobals "1000000"

	# Spawning subshells is expensive so run a background task which we can
	# probe for liveness to determine when time is up.
	local sleepPid
	sleep "$RUNTIME_SEC" &
	sleepPid=$!

	# we are working in nanoseconds (10^9)
	tStart=$(date +%s%N)

	while kill -0 "$sleepPid" 2>&3; do
		emptyBitArray
		runSieve
		((++passes))
	done

	# calculate real runtime
	tRun=$(($(date +%s%N) - tStart))

	printResults "0" "$tRun" "$passes"
}

main
