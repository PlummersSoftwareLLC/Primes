#!/bin/bash
set -e

cargo test --release
cargo run --release

