#!/bin/bash
set -e

cargo test --release
cargo build --release

echo 
echo
echo '"abtracted" version, with results for either bits or bytes for storage'
./target/release/abstracted

echo
echo
echo '"simple-bits" version, very close to the original C++'
./target/release/simple-bits
