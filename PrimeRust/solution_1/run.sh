#!/bin/bash
set -e

cargo test --release
cargo build --release

./target/release/prime-sieve-rust
