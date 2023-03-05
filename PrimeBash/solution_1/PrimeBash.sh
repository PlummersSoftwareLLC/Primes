#! /usr/bin/env bash
#
# A simple prime sieve based on https://github.com/davepl/Primes
# Written in bash.
#
# Tyler Hart (nitepone) <admin@night.horse>

source PrimeBash.common

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

main 'bash'
