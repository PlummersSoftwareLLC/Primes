#/bin/bash
c1541 -format primes,c2 d64 ./primes.d64 -write ./primes.prg
xpet -autostart ./primes.d64 "$@"
