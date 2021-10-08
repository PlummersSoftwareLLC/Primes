#!/bin/sh

gcc primes_arm64_byte.s -o primes_arm64_byte.run
gcc primes_arm64_bitmap.s -o primes_arm64_bitmap.run
gcc primes_arm64_bitshift.s -o primes_arm64_bitshift.run