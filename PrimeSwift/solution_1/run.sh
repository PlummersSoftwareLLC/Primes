#!/bin/sh

swift build --configuration release

./.build/release/PrimeSieveSwift --upper-limit 1000000 --time 5 --list-results false

# Alternative
# ./.build/release/PrimeSieveSwift -n 1000000 -t 5 -l false
