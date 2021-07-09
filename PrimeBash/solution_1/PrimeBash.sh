#! /bin/bash
#
# A simple prime sieve based on https://github.com/davepl/Primes
# Written in bash.
#
# Tyler Hart (nitepone) <admin@night.horse>

# rbergen -- changed number for 10 from 1 to 4
declare -A primeCounts=( \
	[10]=4
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
startArray=()
# rbergen: changed runtime to drag-race default
RUNTIME_SEC=5


function sqrt {
	local i
	i=1
	while ((i*i<=$1)); do
		((i++))
	done
	echo "$((i-1))"
}

function initGlobals {
	sieveSize=$1
	# initialize a bit array of 1s so we can copy it later
	for ((i=0; i<((sieveSize+1) / 2); i++)); do
		startArray[$i]=1
	done
}

function emptyBitArray {
	bitArray=("${startArray[@]}")
}

function validateResults {
	result=$1
	return $(( primeCounts["$sieveSize"] != result ))
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
		echo "You are setting even bits, which is sub-optimal" >&2
		return
	fi
	bitArray[$((index/2))]=0
}

function runSieve {
	local factor
	local q
	factor=3
	q="$(sqrt $sieveSize)"
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
	local dur_nano
	local avg_dur_nano
	local dur_str
	local avg_dur_str
	local passes
	local count
	local valid
	showresults="$1"
	dur_nano="$2"
	passes="$3"
	# create duration strings from nanosecond duration time
	avg_dur_nano=$((dur_nano/passes))
	dur_str="$(printf "%d.%08d"\
		"$((dur_nano/1000000000))"\
		"$((dur_nano%1000000000))"\
	)"
	avg_dur_str="$(printf "%d.%08d"\
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



# we are working in nanoseconds (10^9)
tDiff=$((1000000000*RUNTIME_SEC))
passes=0
initGlobals "1000000"
tStart=$(date +%s%N)
while (( $(date +%s%N) < (tDiff + tStart) )); do
	emptyBitArray
	runSieve
	((passes++))
done

# calculate real runtime
tRun=$(($(date +%s%N) - tStart))

printResults "0" "$tRun" "$passes"
