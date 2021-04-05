#!/bin/bash

crystal build primes.cr --cross-compile --target arm64-apple-darwin --release --no-debug
cc primes.o -v -o primes -fast -L/opt/homebrew/opt/pcre/lib -L/opt/homebrew/opt/bdw-gc/lib -L/opt/homebrew/opt/libevent/lib /Volumes/Data/Projects/crystal/libcrystal.a -levent -liconv -lpthread -ldl -lgc -lpcre