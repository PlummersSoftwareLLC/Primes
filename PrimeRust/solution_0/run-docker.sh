#!/bin/bash
set -e

docker build . -t rust-mike-barber:latest
docker run --rm rust-mike-barber:latest