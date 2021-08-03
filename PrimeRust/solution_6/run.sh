#!/bin/sh
docker build  --rm . -t sycration:rust-impl-const
docker run --rm sycration:rust-impl-const