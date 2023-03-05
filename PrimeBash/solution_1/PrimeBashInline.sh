#! /usr/bin/env bash
#
# A simple prime sieve based on https://github.com/davepl/Primes
# Written in bash.
#
# Tyler Hart (nitepone) <admin@night.horse>

source PrimeBash.common

function runSieve {
	local factor num q
	sqrt q "$sieveSize"
	for ((factor=3; factor <= q; factor+=2)); do
		for ((num=factor; num < sieveSize; num+=2)); do
			if (( !bitArray[\$num] )); then
				factor=$num
				break
			fi
		done
		for ((num=factor**2; num < sieveSize; num+=factor*2)); do
			bitArray[$num]=1
		done
	done
}

main 'bash_inline'
