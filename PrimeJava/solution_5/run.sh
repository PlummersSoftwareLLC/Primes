#!/bin/sh

# The vector loop uses the Vector API, which is still an incubator module. A small young
# generation keeps the garbage, one sieve per pass, close to the caches.
JAVA_OPTS="--add-modules jdk.incubator.vector -XX:+UseSerialGC -Xmn32m -Xlog:disable -Xlog:all=error:stderr"

java $JAVA_OPTS Main
# The same sieve with sun.misc.Unsafe in the "extreme" loop.
java $JAVA_OPTS --sun-misc-unsafe-memory-access=allow Main --unsafe
