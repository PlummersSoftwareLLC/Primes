#!/bin/bash

echo "Requires cargo! Please find this here: https://www.rust-lang.org/tools/install"
cargo clean
cargo build --release
cd ./target/release
./PrimeRust