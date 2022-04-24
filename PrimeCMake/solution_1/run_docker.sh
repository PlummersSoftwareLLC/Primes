#!/bin/sh

docker build --pull --rm -f Dockerfile -t primes_cmake:latest .
docker run --rm primes_cmake:latest
