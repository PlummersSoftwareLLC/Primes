#!/bin/sh

for f in sieve*.go ; do go build --gcflags="-B" -o ./sieves/ $f; done
