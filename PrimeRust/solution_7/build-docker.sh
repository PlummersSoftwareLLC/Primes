#!/bin/bash
set -e

docker build . -t rust-wheel-sieve:latest
