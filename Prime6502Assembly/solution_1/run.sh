#/bin/bash
c1541 -format primes,c1 d64 ./primes.d64 -write ./primes.prg
x128 -autoload ./primes.d64 -keybuf "sys 4864
"
