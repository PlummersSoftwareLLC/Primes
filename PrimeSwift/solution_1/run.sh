#!/bin/sh

set -eu
cd "$(dirname "$0")"

./PrimeSwift_1bit_u8/.build/release/PrimeSieveSwift
./PrimeSwift_1bitStriped_u8/.build/release/PrimeSieveSwift
./PrimeSwift_8bitBool/.build/release/PrimeSieveSwift
