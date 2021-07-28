#!/bin/sh

for pkg in $(ls -d ./*/); do
    swift run --configuration release --disable-sandbox -Xswiftc -O --package-path ${pkg}
done